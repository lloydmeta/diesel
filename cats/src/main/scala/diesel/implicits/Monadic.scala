package diesel.implicits

import cats.Monad
import diesel.{Dsl, MonadicDsl}

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */
trait Monadic {

  implicit def toMonadicDsl[Alg[_[_]], A](dsl: Dsl[Alg, A]): MonadicDsl[Alg, A] =
    new MonadicDsl[Alg, A] {
      def apply[F[_]: Monad](implicit interpreter: Alg[F]): F[A] = dsl.apply(interpreter)
    }

}

object Monadic extends Monadic