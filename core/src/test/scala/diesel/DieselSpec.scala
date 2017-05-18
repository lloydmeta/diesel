package diesel

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class DieselSpec extends FunSpec with Matchers {

  describe("@diesel annotation") {

    describe("Simple Maths DSL") {

      @diesel
      trait Maths[G[_]] {
        def add(l: Int, r: Int): G[Int]

      }

      object Maths {
        implicit val interpreter = new Maths[Id] {
          def add(l: Int, r: Int) = l + r

        }
      }

      import Maths.ops._

      it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
        def prog[F[_]: Monad: Maths](x: Int, y: Int) = {
          Maths.add(x, y)
        }

        prog[Id](3, 19) shouldBe 22
      }

    }

    describe("applicative DSL") {

      // Lifted from https://github.com/adelbertc/tfm/blob/master/examples/src/main/scala/tfm/examples/Free.scala#L8-L21
      @diesel
      trait ApplicativeInterpreter[F[_]] {
        def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
        def pure[A](a: A): F[A]
      }

      implicit def applicative[F[_]](implicit F: Applicative[F]): ApplicativeInterpreter[F] =
        new ApplicativeInterpreter[F] {
          def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
            F.map2(fa, fb)(f)
          def pure[A](a: A): F[A] = F.pure(a)
        }

      import ApplicativeInterpreter.ops._

      def program[F[_]: ApplicativeInterpreter] = {
        ApplicativeInterpreter.map2(ApplicativeInterpreter.pure(1),
                                    ApplicativeInterpreter.pure(2))(_ + _)
      }

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
          def add(l: Int, r: Int): G[Int]
        }

        object Maths {
          val meh: Int = 42

          implicit val interpreter = new Maths[Id] {
            def add(l: Int, r: Int) = l + r
          }
        }

        import Maths.ops._

        it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
          def prog[F[_]: Monad: Maths](x: Int, y: Int) = {
            Maths.add(x, y)
          }

          prog[Id](3, 19) shouldBe 22
        }

      }
    }

    describe("when the annotation is passed an Ops name") {
      describe("Simple Maths DSL") {

        @diesel("Operations")
        trait Maths[G[_]] {
          def add(l: Int, r: Int): G[Int]
        }

        object Maths {
          val answer: Int = 42

          implicit val interpreter = new Maths[Id] {
            def add(l: Int, r: Int) = l + r
          }
        }

        import Maths.Operations._

        it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
          def prog[F[_]: Monad: Maths](x: Int, y: Int) = {
            Maths.add(x, y)
          }

          prog[Id](3, 19) shouldBe 22
        }

      }
    }
    describe("when the annotation is used on an abstract class with a constraint on its type") {
      describe("Simple Maths DSL") {

        @diesel("Operations")
        abstract class Maths[G[_]: Monad] {
          def add(l: Int, r: Int): G[Int]
        }

        object Maths {
          val answer: Int = 42

          implicit val interpreter = new Maths[Option] {
            def add(l: Int, r: Int) = Some(l + r)
          }
        }

        import Maths.Operations._

        it("should expand a trait into an object holding Algebra and DSL wrapper methods") {
          def prog[F[_]: Monad: Maths](x: Int, y: Int) = {
            Maths.add(x, y)
          }

          prog[Option](3, 19) shouldBe Some(22)
        }

      }
    }

  }

  describe("composing multiple DSLs") {

    @diesel
    trait Maths[G[_]] {
      def add(l: Int, r: Int): G[Int]

    }

    @diesel
    trait Logger[F[_]] {
      def info(s: => String): F[Unit]
    }

    import Maths.ops._, Logger.ops._

    def loggedAdd[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
      for {
        s <- Maths.add(x, y)
        _ <- Logger.info(s"Sum was $s")
      } yield s
    }

    implicit val MathsIdInterp = new Maths[Id] {
      def add(l: Int, r: Int) = l + r
    }

    implicit val LoggerIdInterp = new Logger[Id] {
      def info(s: => String) = println(s)
    }

    it("should work as expected") {
      loggedAdd[Id](3, 10) shouldBe 13
    }
  }

}
