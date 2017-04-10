package diesel.implicits

import scalaz._
import diesel.diesel
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */
class ImplicitsSpec extends FunSpec with Matchers {

  describe("composing inside a monadic environment") {

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

    class Environment[M[_]: Monad: MonadPlus](implicit mathsInterp: Maths.Algebra[M],
                                              applicInterp: ApplicativeInterpreter.Algebra[M]) {

      import scalaz.Scalaz._

      import Maths._
      import ApplicativeInterpreter._

      def op(a: Int, b: Int, c: Int): M[Int] =
        for {
          s <- add(int(a), int(b))
          l <- pure(s)
          if l > 3
          r <- add(int(l), int(c))
        } yield r

      def plus(a: Int, b: Int): M[Int] = add(int(a), int(b)).flatMap(pure(_))
    }

    describe("evaluating using the environment") {

      import scalaz.Scalaz._

      implicit def maths[F[_]](implicit F: Monad[F]): Maths.Algebra[F] = new Maths.Algebra[F] {
        def int(i: Int) = F.pure(i)
        def add(l: F[Int], r: F[Int]) =
          for {
            x <- l
            y <- r
          } yield x + y
      }

      implicit def applicative[F[_]](
          implicit F: Applicative[F]): ApplicativeInterpreter.Algebra[F] =
        new ApplicativeInterpreter.Algebra[F] {
          def pure[A](a: A): F[A]                                     = F.pure(a)
          def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = F.apply2(fa, fb)(f)
        }

      val optEnv  = new Environment[Option]
      val listEnv = new Environment[List]

      it("should work") {
        optEnv.op(1, 2, 3) shouldBe None
        optEnv.op(4, 4, 3) shouldBe Some(11)
        listEnv.op(1, 2, 3) shouldBe Nil
        listEnv.op(4, 4, 3) shouldBe List(11)
      }
    }

  }
}
