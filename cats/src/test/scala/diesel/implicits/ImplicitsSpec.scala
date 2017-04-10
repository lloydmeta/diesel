package diesel.implicits

import cats.{Applicative, Monad, MonadFilter}
import diesel.diesel
import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds

class ImplicitsSpec extends FunSpec with Matchers {

  describe("composing languages using the implicits") {

    @diesel
    trait Maths[G[_]] {
      def int(i: Int): G[Int]
      def add(l: G[Int], r: G[Int]): G[Int]
    }

    @diesel
    trait Applicative[F[_]] {
      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
      def pure[A](a: A): F[A]
    }

    // Our combined algebra type
    type PRG[A[_]] = Applicative.Algebra[A] with Maths.Algebra[A]

    import Maths._
    import Applicative._
    import cats.implicits._

    val monadicPlusOp = { (a: Int, b: Int, c: Int) =>
      import monadicplus._
      for {
        i <- add(int(a), int(b)).withAlg[PRG]
        if i > 3
        j <- pure(c).withAlg[PRG]
        k <- add(int(i), int(j)).withAlg[PRG]
      } yield k
    }

    val monadicOp = { (a: Int, b: Int, c: Int) =>
      import monadic._
      for {
        i <- add(int(a), int(b)).withAlg[PRG]
        j <- pure(c).withAlg[PRG]
        k <- add(int(i), int(j)).withAlg[PRG]
      } yield k
    }

    implicit def interp[F[_]](implicit F: Monad[F]) =
      new Applicative.Algebra[F] with Maths.Algebra[F] {
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

    describe("using monadic") {

      it("should work") {
        val program1 = monadicOp(1, 2, 3)
        val program2 = monadicOp(3, 4, 5)
        program1[Option] shouldBe Some(6)
        program1[List] shouldBe List(6)
        program2[Option] shouldBe Some(12)
        program2[List] shouldBe List(12)
      }
    }
    describe("using monadicplus") {

      it("should work") {
        val program1 = monadicPlusOp(1, 2, 3)
        val program2 = monadicPlusOp(3, 4, 5)
        program1[Option] shouldBe None
        program1[List] shouldBe Nil
        program2[Option] shouldBe Some(12)
        program2[List] shouldBe List(12)
      }
    }

  }
}
