package diesel

import cats.Monad

import scala.language.higherKinds

trait MonadicDsl[Alg[_[_]], A] { self =>

  import cats.implicits._

  def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[A]

  def map[B](f: A => B): MonadicDsl[Alg, B] = new MonadicDsl[Alg, B] {
    def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[B] = {
      self[F].map(f)
    }
  }

  def flatMap[AlgB[_[_]], B](f: A => MonadicDsl[AlgB, B])
    : MonadicDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, B] =
    new MonadicDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, B] {
      def apply[F[_]: Monad](implicit interpreter: Alg[F] with AlgB[F]): F[B] = {
        self[F].flatMap(r => f(r)[F])
      }
    }

}
