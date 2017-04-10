package diesel.implicits

import cats.Monad
import diesel.Dsl

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * Implicity converts DSLs to MonadicDSl so that you can use them in for-comprehensions.
  *
  * The catch is that the interpreter that you use at the end must be written for a Monad
  */
object monadic extends monadic

trait monadic {

  implicit def toMonadicDsl[Alg[_[_]], A](dsl: Dsl[Alg, A]): MonadicDsl[Alg, A] =
    new MonadicDsl[Alg, A] {
      def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[A] = dsl.apply(interpreter)
    }

}

trait MonadicDsl[Alg[_[_]], A] { self =>

  import cats.implicits._

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
