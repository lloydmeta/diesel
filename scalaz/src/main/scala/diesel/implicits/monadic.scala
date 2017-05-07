package diesel.implicits

import scalaz.{Bind, Monad}
import diesel.Dsl

import scala.language.implicitConversions
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
  *      |  trait Applicatives[F[_]] {
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  *      |    def pure[A](a: A): F[A]
  *      | } }
  *
  * // Import the stuff we've just built
  * scala> import Wrapper._
  * scala> import Maths.Ops._
  * scala> import Applicatives.Ops._
  * scala> import scalaz.Monad
  *
  * // Our combined algebra type and our program that uses it
  * scala> def op[F[_]: Monad: Maths: Applicatives](a: Int, b: Int, c: Int) = {
  *      |    import monadic._
  *      |    // Note the use of for comprehensions in here
  *      |    for {
  *      |      i <- add(int(a), int(b))
  *      |      j <- pure(c)
  *      |      k <- add(int(i), int(j))
  *      |    } yield k
  *      | }
  *
  * scala> import scalaz.Scalaz._
  * // Write our interpreter  *
  * scala> implicit def interp[F[_]](implicit F: Monad[F]) = new Applicatives[F] with Maths[F] {
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
  * scala> op[Option](1, 2, 3)
  * res0: Option[Int] = Some(6)
  * }}}
  */
object monadic extends MonadicDsl with MonadicF

trait MonadicDsl {

  /**
    * Automatic conversion from [[diesel.Dsl]] to Monad.AllOps for a DSL if there is a suitable interpreter in scope
    */
  implicit def DslToMonadicOps[Alg[_[_]], A, F[_]: Monad: Alg](
      dsl: Dsl[Alg, A]): MonadicOps[F, A] = new MonadicOps(dsl.apply[F])

}

trait MonadicF {

  /**
    * Automatic conversion from a F[_] to Monad.AllOps for a DSL if there is a suitable interpreter in scope
    */
  implicit def FtoMonadic[F[_]: Monad, A](f: F[A]): MonadicOps[F, A] = new MonadicOps(f)

}

/**
  * Enrichment class for Monadic F[_]
  */
class MonadicOps[F[_]: Monad, A] private[implicits] (m: F[A]) {

  def map[B](f: A => B): F[B] = Monad[F].map(m)(f)

  def flatMap[B](f: A => F[B]): F[B] = Bind[F].bind(m)(f)

}
