package diesel

import cats.MonadFilter

import scala.language.{higherKinds}

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */

trait MonadicFilterDsl[Alg[_[_]], A] { self =>

  import cats.implicits._

  def apply[F[_]: MonadFilter](implicit interpreter: Alg[F]): F[A]

  def map[B](f: A => B): MonadicFilterDsl[Alg, B] = new MonadicFilterDsl[Alg, B] {
    def apply[F[_]: MonadFilter](implicit interpreter: Alg[F]): F[B] = {
      self[F].map(f)
    }
  }

  def flatMap[AlgB[_[_]], B](f: A => MonadicFilterDsl[AlgB, B])
    : MonadicFilterDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, B] =
    new MonadicFilterDsl[({ type Combined[X[_]] = Alg[X] with AlgB[X] })#Combined, B] {
      def apply[F[_]: MonadFilter](implicit interpreter: Alg[F] with AlgB[F]): F[B] = {
        self[F].flatMap(r => f(r)[F])
      }
    }

  def filter(f: A => Boolean): MonadicFilterDsl[Alg, A] = new MonadicFilterDsl[Alg, A] {
    def apply[F[_]: MonadFilter](implicit interpreter: Alg[F]): F[A] = {
      implicitly[MonadFilter[F]].filter(self[F])(f)
    }
  }

  def filterWith(f: A => Boolean): MonadicFilterDsl[Alg, A] = filter(f)

}
