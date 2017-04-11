package diesel.implicits

import cats.Monad
import diesel.Dsl

import scala.language.higherKinds

/**
  * Implicitly converts DSLs to MonadicDSl so that you can use them in for-comprehensions.
  *
  * The catch is that the interpreter that you use at the end must be written for a Monad
  *
  * Example:
  *
  * {{{
  * scala> import _root_.diesel._
  * scala> import scala.language.higherKinds
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
  * scala> import cats.Monad
  * scala> import cats.implicits._
  *
  * // Our combined algebra type and our program that uses it
  * scala> type PRG[A[_]] = Applicative.Algebra[A] with Maths.Algebra[A]
  * scala> val op = { (a: Int, b: Int, c: Int) =>
  *      |    import monadic._
  *      |    // Note the use of for comprehensions in here
  *      |    for {
  *      |      i <- add(int(a), int(b)).withAlg[PRG]
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
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = F.map2(fa, fb)(f)
  *      |    def pure[A](a: A): F[A] = F.pure(a)
  *      | }
  *
  * // Now we can use our DSL
  * scala> val program = op(1, 2, 3)
  *
  * scala> program[Option]
  * res0: Option[Int] = Some(6)
  * }}}
  */
object monadic extends monadic

trait monadic {

  implicit class DslToMonadicDsl[Alg[_[_]], A](dsl: Dsl[Alg, A]) extends MonadicDsl[Alg, A] {
    def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[A] = dsl.apply(interpreter)
  }

}

trait MonadicDsl[Alg[_[_]], A] { self =>

  import cats.implicits._

  /**
    * Evaluate this Dsl to a F[A]
    */
  def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[A]

  def map[B](f: A => B): MonadicDsl[Alg, B] = new MonadicDsl[Alg, B] {
    def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[B] = {
      self[F].map(f)
    }
  }

  /**
    * Combines Alg with AlgB
    *
    * Useful for flatmapping and for-comprehensions in general
    */
  def withAlg[AlgB[_[_]]]
    : MonadicDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] =
    new MonadicDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] {
      def apply[F[_]: Monad](implicit interpreter: Alg[F] with AlgB[F]): F[A] = {
        self[F]
      }
    }

  def flatMap[B](f: A => MonadicDsl[Alg, B]): MonadicDsl[Alg, B] =
    new MonadicDsl[Alg, B] {
      def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[B] = {
        self[F].flatMap(r => f(r)[F])
      }
    }

}
