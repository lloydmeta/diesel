package diesel.implicits

import cats.Monad
import diesel.Dsl

import scala.language.higherKinds
import scala.language.implicitConversions

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
  *      |  trait Applicatives[F[_]] {
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  *      |    def pure[A](a: A): F[A]
  *      | } }
  *
  * // Import the stuff we've just built
  * scala> import Wrapper._
  * scala> import Maths.Ops._
  * scala> import Applicatives.Ops._
  * scala> import cats.Monad
  *
  * // Our combined algebra type and our program that uses it
  * scala> def op[A[_]: Monad: Applicatives: Maths](a: Int, b: Int, c: Int) = {
  *      |    import monadic._
  *      |    // Note the use of for comprehensions in here
  *      |    for {
  *      |      i <- add(int(a), int(b))
  *      |      j <- pure(c)
  *      |      k <- add(int(i), int(j))
  *      |    } yield k
  *      | }
  *
  * scala> import cats.implicits._
  * // Write our interpreter
  * scala> implicit def interp[F[_]](implicit F: Monad[F]) = new Applicatives[F] with Maths[F] {
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
  * scala> op[Option](1, 2, 3)
  * res0: Option[Int] = Some(6)
  * }}}
  */
object monadic extends MonadicDsl with MonadicF

trait MonadicDsl {

  /**
    * Automatic conversion from [[diesel.Dsl]] to Monad.AllOps for a DSL if there is a suitable interpreter in scope
    */
  implicit def DslToMonadic[Alg[_[_]], A, F[_]: Monad: Alg](dsl: Dsl[Alg, A]): Monad.AllOps[F, A] =
    Monad.ops.toAllMonadOps(dsl.apply[F])

}

trait MonadicF {

  /**
    * Automatic conversion from a F[_] to Monad.AllOps for a DSL if there is a suitable interpreter in scope
    */
  implicit def FtoMonadic[F[_]: Monad, A](f: F[A]): Monad.AllOps[F, A] = Monad.ops.toAllMonadOps(f)

}
