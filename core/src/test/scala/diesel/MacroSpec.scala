package diesel

import cats.Applicative
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Lloyd on 4/7/17.
  *
  * Copyright 2017
  */
class MacroSpec extends FunSpec with Matchers {

  type Id[A] = A

  describe("@diesel annotation") {

    describe("Simple Maths DSL") {

      object Maths {

        import scala.language.higherKinds

        trait Algebra[G[_]] {
          def int(i: Int): G[Int]

          def add(l: G[Int], r: G[Int]): G[Int]

          def optInt(i: Option[Int]): G[Option[Int]]

          def wrappedInt(i: Int): G[Int] = self.int(i)

          protected[diesel] def mixedInts[H[_]](i: Int,
                                                optInt: Option[Int],
                                                gInt: G[Int],
                                                hInt: H[Int]): G[Int]
        }

        def int(i: Int): _root_.diesel.Dsl[Algebra, Int] = new _root_.diesel.Dsl[Algebra, Int] {
          def apply[G[_]](implicit I: Algebra[G]): G[Int] = I.int(i)
        }

        def add(l: _root_.diesel.Dsl[Algebra, Int],
                r: _root_.diesel.Dsl[Algebra, Int]): _root_.diesel.Dsl[Algebra, Int] =
          new _root_.diesel.Dsl[Algebra, Int] {
            def apply[G[_]](implicit I: Algebra[G]): G[Int] = I.add(l.apply[G], r.apply[G])
          }

        def optInt(i: Option[Int]): _root_.diesel.Dsl[Algebra, Option[Int]] =
          new _root_.diesel.Dsl[Algebra, Option[Int]] {
            def apply[G[_]](implicit I: Algebra[G]): G[Option[Int]] = I.optInt(i)
          }

        protected[diesel] def mixedInts[H[_]](i: Int,
                                              optInt: Option[Int],
                                              gInt: _root_.diesel.Dsl[Algebra, Int],
                                              hInt: H[Int]): _root_.diesel.Dsl[Algebra, Int] =
          new _root_.diesel.Dsl[Algebra, Int] {
            def apply[G[_]](implicit I: Algebra[G]): G[Int] =
              I.mixedInts(i, optInt, gInt.apply[G], hInt)
          }

        def wrappedInt(i: Int): _root_.diesel.Dsl[Algebra, Int] =
          new _root_.diesel.Dsl[Algebra, Int] {
            def apply[G[_]](implicit I: Algebra[G]): G[Int] = I.wrappedInt(i)
          }
      }

      val interpreter = new Maths.Algebra[Id] {
        def int(i: Int)                                                               = i
        def add(l: Id[Int], r: Id[Int])                                               = l + r
        def optInt(i: Option[Int])                                                    = i
        def mixedInts[H[_]](i: Int, optInt: Option[Int], gInt: Id[Int], hInt: H[Int]) = i
      }

      import Maths._

      it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
        int(3)(interpreter) shouldBe 3
        add(int(3), int(10))(interpreter) shouldBe 13
      }

    }

    describe("applicative DSL") {

      // Lifted from https://github.com/adelbertc/tfm/blob/master/examples/src/main/scala/tfm/examples/Free.scala#L8-L21
      @diesel
      trait ApplicativeInterpreter[F[_]] {
        def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
        def pure[A](a: A): F[A]
      }

      def applicative[F[_]](implicit F: Applicative[F]): ApplicativeInterpreter.Algebra[F] =
        new ApplicativeInterpreter.Algebra[F] {
          def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
            F.map2(fa, fb)(f)
          def pure[A](a: A): F[A] = F.pure(a)
        }

      import ApplicativeInterpreter._
      import cats.implicits._

      val program = map2(pure(1), pure(2))(_ + _)

      it("should work with Option") {
        val id = program(applicative[Id])
        id shouldBe 3
        val list = program(applicative[List])
        list shouldBe List(3)
        val option = program(applicative[Option])
        option shouldBe Some(3)
      }

    }

    describe("when the annotated trait already has a companion object") {
      describe("Simple Maths DSL") {

        @diesel
        trait Maths[G[_]] {
          def int(i: Int): G[Int]
          def add(l: G[Int], r: G[Int]): G[Int]
        }

        object Maths {
          val answer: Int = 42

          val interpreter = new Algebra[Id] {
            def int(i: Int)                 = i
            def add(l: Id[Int], r: Id[Int]) = l + r
          }
        }

        import Maths._

        it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
          int(3)(interpreter) shouldBe 3
          add(int(3), int(10))(interpreter) shouldBe 13
        }

      }
    }

    describe("when the annotation is passed an Algebra name") {
      describe("Simple Maths DSL") {

        @diesel("Alg")
        trait Maths[G[_]] {
          def int(i: Int): G[Int]
          def add(l: G[Int], r: G[Int]): G[Int]
        }

        object Maths {
          val answer: Int = 42

          val interpreter = new Alg[Id] {
            def int(i: Int)                 = i
            def add(l: Id[Int], r: Id[Int]) = l + r
          }
        }

        import Maths._

        it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
          int(3)(interpreter) shouldBe 3
          add(int(3), int(10))(interpreter) shouldBe 13
        }

      }
    }

  }

}
