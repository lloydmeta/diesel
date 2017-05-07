package diesel.implicits

import cats.MonadFilter
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
  * scala> import cats.{Monad, MonadFilter}
  *
  * // Our combined algebra type and our program that uses it
  * scala> def op[A[_]: MonadFilter: Maths: Applicatives](a: Int, b: Int, c: Int) = {
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
  * res0: Option[Int] = None
  *
  * scala> op[Option](4, 5, 6)
  * res1: Option[Int] = Some(15)
  * }}}
  */
object monadicplus extends MonadicPlusDsl with MonadicPlusF

trait MonadicPlusDsl {

  implicit def DslToMonadicFilter[Alg[_[_]], A, F[_]: MonadFilter: Alg](
      dsl: Dsl[Alg, A]): HasWithFilter[F, A] =
    new HasWithFilter(MonadFilter.ops.toAllMonadFilterOps(dsl.apply[F]))

}

trait MonadicPlusF {

  implicit def FToMonadicFilter[A, F[_]: MonadFilter](f: F[A]): HasWithFilter[F, A] =
    new HasWithFilter(MonadFilter.ops.toAllMonadFilterOps(f))
}

class HasWithFilter[F[_], A](underlying: MonadFilter.AllOps[F, A])
    extends MonadFilter.AllOps[F, A] {

  def withFilter(f: A => Boolean): F[A] = underlying.filter(f)

  val typeClassInstance: MonadFilter[F] = underlying.typeClassInstance

  def self: F[A] = underlying.self
}
