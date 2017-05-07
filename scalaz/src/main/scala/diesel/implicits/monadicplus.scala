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
  *      |  trait Applicatives[F[_]] {
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  *      |    def pure[A](a: A): F[A]
  *      | } }
  *
  * // Import the stuff we've just built
  * scala> import Wrapper._
  * scala> import Maths.Ops._
  * scala> import Applicatives.Ops._
  * scala> import scalaz.MonadPlus
  *
  * // Our combined algebra type and our program that uses it
  * scala> def op[F[_]: MonadPlus: Maths: Applicatives](a: Int, b: Int, c: Int) = {
  *      |    import monadicplus._
  *      |    // Note the use of for comprehensions in here
  *      |    for {
  *      |      i <- add(int(a), int(b))
  *      |      if i > 3
  *      |      j <- pure(c)
  *      |      k <- add(int(i), int(j))
  *      |    } yield k
  *      | }
  *
  * scala> import scalaz.Scalaz._
  * // Write our interpreter
  * scala> implicit def interp[F[_]: MonadPlus] = new Applicatives[F] with Maths[F] {
  *      |    def int(i: Int) = MonadPlus[F].pure(i)
  *      |    def add(l: F[Int], r: F[Int]) =
  *      |      for {
  *      |        x <- l
  *      |        y <- r
  *      |      } yield x + y
  *      |    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = MonadPlus[F].apply2(fa, fb)(f)
  *      |    def pure[A](a: A): F[A] = MonadPlus[F].pure(a)
  *      | }
  *
  * // Now we can use our DSL
  * scala> op[Option](1, 2, 3)
  * res0: Option[Int] = None
  *
  * scala> op[Option](4, 5, 6)
  * res1: Option[Int] = Some(15)
  * }}}
  */
object monadicplus extends MonadicPlusDsl with MonadicPlusF

trait MonadicPlusDsl {

  implicit def DslToMonadicPlusDsl[Alg[_[_]], A, F[_]: MonadPlus: Alg](
      dsl: Dsl[Alg, A]): MonadicPlusOps[F, A] = new MonadicPlusOps(dsl.apply[F])

}

trait MonadicPlusF {

  /**
    * Automatic conversion from a F[_] to Monad.AllOps for a DSL if there is a suitable interpreter in scope
    */
  implicit def FtoMonadicPlus[F[_]: MonadPlus, A](f: F[A]): MonadicPlusOps[F, A] =
    new MonadicPlusOps(f)

}

final class MonadicPlusOps[F[_]: MonadPlus, A] private[implicits] (m: F[A])
    extends MonadicOps[F, A](m) {

  def filter(f: A => Boolean): F[A] = MonadPlus[F].filter(m)(f)

  def withFilter(f: A => Boolean): F[A] = filter(f)

}
