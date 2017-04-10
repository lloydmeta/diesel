package diesel.implicits

import scalaz.MonadPlus
import diesel.Dsl

import scala.language.higherKinds

/**
  * Implicity converts DSLs to MonadicPlusDsl so that you can use them in for-comprehensions.
  *
  * Note, this is more powerful than importing from monadic because this requires a MonadPlus instance
  * for the F[_] in the eventual interpreter that you use to get your results.
  */
object monadicplus extends monadicplus

trait monadicplus {

  implicit def dslToMonadicFilterDsl[Alg[_[_]], A](dsl: Dsl[Alg, A]): MonadPlusDsl[Alg, A] =
    new MonadPlusDsl[Alg, A] {
      def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A] = dsl[F]
    }

}

/**
  * Allows for the full span of for-comprehension options to be used, including
  * filtering in between.
  *
  * However, requires there to be a MonadFilter instance for the F[_] that your eventual
  * interpreter will use.
  */
trait MonadPlusDsl[Alg[_[_]], A] { self =>

  import scalaz.Scalaz._

  def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A]

  def map[B](f: A => B): MonadPlusDsl[Alg, B] = new MonadPlusDsl[Alg, B] {
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
    : MonadPlusDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] =
    new MonadPlusDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, A] {
      def apply[F[_]: MonadPlus](implicit interpreter: Alg[F] with AlgB[F]): F[A] = {
        self[F]
      }
    }

  def flatMap[B](f: A => MonadPlusDsl[Alg, B]): MonadPlusDsl[Alg, B] =
    new MonadPlusDsl[Alg, B] {
      def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[B] = {
        self[F].flatMap(r => f(r)[F])
      }
    }

  def filter(f: A => Boolean): MonadPlusDsl[Alg, A] = new MonadPlusDsl[Alg, A] {
    def apply[F[_]: MonadPlus](implicit interpreter: Alg[F]): F[A] = {
      implicitly[MonadPlus[F]].filter(self[F])(f)
    }
  }

  def withFilter(f: A => Boolean): MonadPlusDsl[Alg, A] = filter(f)

}
