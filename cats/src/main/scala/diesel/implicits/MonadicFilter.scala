package diesel.implicits

import cats.MonadFilter
import diesel.{Dsl, MonadicFilterDsl}

import scala.language.higherKinds

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */
trait MonadicFilter {

  implicit def toMonadicFilterDsl[Alg[_[_]], A](dsl: Dsl[Alg, A]): MonadicFilterDsl[Alg, A] =
    new MonadicFilterDsl[Alg, A] {
      def apply[F[_]: MonadFilter](implicit interpreter: Alg[F]): F[A] = dsl[F]
    }

}

object MonadicFilter extends MonadicFilter
