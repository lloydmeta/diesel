package diesel.implicits

import scalaz.{Monad, MonadPlus}
import diesel.diesel
import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds

/**
  * Created by Lloyd on 4/10/17.
  *
  * Copyright 2017
  */
class ImplicitsSpec extends FunSpec with Matchers {

  describe("composing languages using the implicits") {

    @diesel
    trait Maths[G[_]] {
      def int(i: Int): G[Int]
      def add(l: G[Int], r: G[Int]): G[Int]
    }

    @diesel
    trait Applicatives[F[_]] {
      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
      def pure[A](a: A): F[A]
    }

    @diesel
    trait Logging[F[_]] {
      def info(s: String): F[Unit]
      def warn(s: String): F[Unit]
      def error(s: String): F[Unit]
    }

    import Maths.Ops._
    import Applicatives.Ops._
    import Logging.{Ops => LoggingOps}

    // Our combined applicative and maths algebra type
    def monadicPlusOp[A[_]: MonadPlus: Applicatives: Maths](a: Int, b: Int, c: Int) = {
      import monadicplus._
      for {
        i <- add(int(a), int(b))
        if i > 3
        j <- pure(c)
        k <- add(int(i), int(j))
      } yield k
    }

    // Composing a composed DSL...
    // Our combined applicative and maths *and* logging algebras
    def monadicPlusOpWithWarn[A[_]: MonadPlus: Applicatives: Maths: Logging](a: Int,
                                                                             b: Int,
                                                                             c: Int) = {
      import monadicplus._
      for {
        v <- monadicPlusOp(a, b, c)
        _ <- LoggingOps.warn(v.toString)
      } yield v
    }

    def monadicOp[F[_]: Monad: Applicatives: Maths: Logging](a: Int, b: Int, c: Int) = {
      import monadic._
      for {
        i <- add(int(a), int(b))
        j <- pure(c)
        _ <- LoggingOps.info(j.toString)
        k <- add(int(i), int(j))
      } yield k
    }

    def monadicToMonadicPlusOp[A[_]: MonadPlus: Applicatives: Maths: Logging](a: Int,
                                                                              b: Int,
                                                                              c: Int) = {
      import monadicplus._
      for {
        i <- monadicOp(a, b, c)
        if i > 0
        _ <- LoggingOps.info(i.toString)
      } yield i
    }

    import scalaz.Scalaz._
    implicit def interp[F[_]](implicit F: Monad[F]) =
      new Applicatives[F] with Maths[F] with Logging[F] {
        def int(i: Int) = F.pure(i)
        def add(l: F[Int], r: F[Int]) =
          for {
            x <- l
            y <- r
          } yield x + y

        def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
          F.apply2(fa, fb)(f)
        def pure[A](a: A): F[A] = F.pure(a)
        def info(s: String)     = F.pure(println(s"INFO: $s"))
        def warn(s: String)     = F.pure(println(s"WARN: $s"))
        def error(s: String)    = F.pure(println(s"ERROR: $s"))
      }

    describe("using monadic") {

      it("should work") {
        def program1[F[_]: Monad] = monadicOp[F](1, 2, 3)
        def program2[F[_]: Monad] = monadicOp[F](3, 4, 5)
        program1[Option] shouldBe Some(6)
        program1[List] shouldBe List(6)
        program2[Option] shouldBe Some(12)
        program2[List] shouldBe List(12)
      }
    }

    describe("using monadicplus") {

      it("should work") {
        def program1[F[_]: MonadPlus] = monadicPlusOp[F](1, 2, 3)
        def program2[F[_]: MonadPlus] = monadicPlusOp[F](3, 4, 5)
        def program3[F[_]: MonadPlus] = monadicPlusOpWithWarn[F](3, 4, 5)
        program1[Option] shouldBe None
        program1[List] shouldBe Nil
        program2[Option] shouldBe Some(12)
        program2[List] shouldBe List(12)
        program3[Option] shouldBe Some(12)
        program3[List] shouldBe List(12)
      }
    }

    describe("converting from monadic to monadicplus") {
      it("should work") {
        def program1[F[_]: MonadPlus] = monadicToMonadicPlusOp[F](1, 2, 3)
        def program2[F[_]: MonadPlus] = monadicToMonadicPlusOp[F](3, 4, 5)
        program1[Option] shouldBe Some(6)
        program1[List] shouldBe List(6)
        program2[Option] shouldBe Some(12)
        program2[List] shouldBe List(12)
      }
    }

  }
}
