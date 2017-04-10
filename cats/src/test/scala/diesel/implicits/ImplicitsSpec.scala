package diesel.implicits

import cats.{Applicative, Monad, MonadFilter}
import diesel.diesel
import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */
class ImplicitsSpec extends FunSpec with Matchers {

  describe("composing as monads") {

    @diesel
    trait Maths[G[_]] {
      def int(i: Int): G[Int]
      def add(l: G[Int], r: G[Int]): G[Int]
    }

    @diesel
    trait ApplicativeInterpreter[F[_]] {
      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
      def pure[A](a: A): F[A]
    }

    def op(a: Int, b: Int, c: Int) = {
      import Maths._
      import ApplicativeInterpreter._
      add(int(a), int(b))
        .filter(_ > 3)
        .flatMap(i => add(int(i), int(c)).flatMap(y => add(int(y), int(y))))
    }

    describe("evaluating using the environment") {

      import cats.implicits._

      implicit def interp[F[_]](implicit F: Monad[F]) =
        new Maths.Algebra[F] with ApplicativeInterpreter.Algebra[F] {
          import cats.implicits._
          def int(i: Int) = F.pure(i)
          def add(l: F[Int], r: F[Int]) =
            for {
              x <- l
              y <- r
            } yield x + y

          def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
            F.map2(fa, fb)(f)
          def pure[A](a: A): F[A] = F.pure(a)
        }

      it("should work") {
        val program1 = op(1, 2, 3)
        val program2 = op(3, 4, 5)
        program1[Option] shouldBe None
      }
    }

  }
}
