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

      @diesel
      trait Maths[F[_]] {
        def int(i: Int): F[Int]
        def add(l: F[Int], r: F[Int]): F[Int]
      }

      val interpreter = new Maths.Algebra[Id] {
        def int(i: Int)                 = i
        def add(l: Id[Int], r: Id[Int]) = l + r
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

  }

}
