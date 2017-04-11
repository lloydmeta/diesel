package diesel.implicits

import scalaz.MonadPlus
import diesel.Dsl

import scala.language.higherKinds

/**
  * Implicitly converts DSLs to MonadicPlusDsl so that you can use them in for-comprehensions.
  *
  * Note, this is more powerful than importing from monadic because this requires a MonadPlus instance
  * for the F[_] in the eventual interpreter that you use to get your results.
  *
  * Example:
  *
  * {{{
  * scala> import _root_.diesel._
  *
  * // Wrapper is only for the sake of sbt-doctest and is unnecessary in real-life usage
  * scala> object Wrapper {
  *      | // Declare our DSLs
  *      | @diesel
  *      | trait Maths[G[_]] {
  *      |   def int(i: Int): G[Int]
  *      |   def add(l: G[Int], r: G[Int]): G[Int]
  *      | }
  *      | @diesel
  *      |  trait Applicative[F[_]] {
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  *      |    def pure[A](a: A): F[A]
  *      | } }
  *
  * // Import the stuff we've just built
  * scala> import Wrapper._
  * scala> import Maths._
  * scala> import Applicative._
  * scala> import scalaz.Monad
  * scala> import scalaz.Scalaz._
  *
  * // Our combined algebra type and our program that uses it
  * scala> type PRG[A[_]] = Applicative.Algebra[A] with Maths.Algebra[A]
  * scala> val op = { (a: Int, b: Int, c: Int) =>
  *      |    import monadicplus._
  *      |    // Note the use of for comprehensions in here
  *      |    for {
  *      |      i <- add(int(a), int(b)).withAlg[PRG]
  *      |      if i > 3
  *      |      j <- pure(c).withAlg[PRG]
  *      |      k <- add(int(i), int(j)).withAlg[PRG]
  *      |    } yield k
  *      | }

  * // Write our interpreter
  * scala> implicit def interp[F[_]](implicit F: Monad[F]) = new Applicative.Algebra[F] with Maths.Algebra[F] {
  *      |    def int(i: Int) = F.pure(i)
  *      |    def add(l: F[Int], r: F[Int]) =
  *      |      for {
  *      |        x <- l
  *      |        y <- r
  *      |      } yield x + y
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = F.apply2(fa, fb)(f)
  *      |    def pure[A](a: A): F[A] = F.pure(a)
  *      | }
  *
  * // Now we can use our DSL
  * scala> val program1 = op(1, 2, 3)
  * scala> val program2 = op(4, 5, 6)
  *
  * scala> program1[Option]
  * res0: Option[Int] = None
  *
  * scala> program2[Option]
  * res1: Option[Int] = Some(15)
  * }}}
  */
object monadicplus extends monadicplus

trait monadicplus {

  implicit class DslToMonadicPlusDsl[Alg[_[_]], A](dsl: Dsl[Alg, A])
      extends MonadicPlusDsl[Alg, A] {
    def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A] = dsl[F]
  }

}

class MonadicDslToMonadicPlusDsl[Alg[_[_]], A](dsl: MonadicDsl[Alg, A])
    extends MonadicPlusDsl[Alg, A] {
  def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A] = dsl[F]
}

/**
  * Allows for the full span of for-comprehension options to be used, including
  * filtering in between.
  *
  * However, requires there to be a MonadFilter instance for the F[_] that your eventual
  * interpreter will use.
  */
trait MonadicPlusDsl[Alg[_[_]], A] { self =>

  import scalaz.Scalaz._

  def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A]

  def map[B](f: A => B): MonadicPlusDsl[Alg, B] = new MonadicPlusDsl[Alg, B] {
    def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[B] = {
      self[F].map(f)
    }
  }

  /**
    * Combines Alg with AlgB
    *
    * Useful for flatmapping and for-comprehensions in general
    */
  def withAlg[AlgB[_[_]]]
    : MonadicPlusDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] =
    new MonadicPlusDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] {
      def apply[F[_]: MonadPlus](implicit interpreter: Alg[F] with AlgB[F]): F[A] = {
        self[F]
      }
    }

  def flatMap[B](f: A => MonadicPlusDsl[Alg, B]): MonadicPlusDsl[Alg, B] =
    new MonadicPlusDsl[Alg, B] {
      def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[B] = {
        self[F].flatMap(r => f(r)[F])
      }
    }

  def filter(f: A => Boolean): MonadicPlusDsl[Alg, A] = new MonadicPlusDsl[Alg, A] {
    def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A] = {
      implicitly[MonadPlus[F]].filter(self[F])(f)
    }
  }

  def withFilter(f: A => Boolean): MonadicPlusDsl[Alg, A] = filter(f)

}
