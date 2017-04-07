package diesel

import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Lloyd on 4/7/17.
  *
  * Copyright 2017
  */
class MacroSpec extends FunSpec with Matchers {

  @diesel
  trait Maths[F[_]] {
    def int(i: Int): F[Int]
    def add(l: F[Int], r: F[Int]): F[Int]
  }

  type Id[A] = A
  val interpreter = new Maths.Algebra[Id] {
    def int(i: Int) = i
    def add(l: Id[Int], r: Id[Int]) = l + r
  }

  describe("@diesel annotation") {

    import Maths._

    it(
      "should expand a trait into an object holding Algebra and DSL wrapper methods") {
      int(3)(interpreter) shouldBe 3
      add(int(3), int(10))(interpreter) shouldBe 13
    }

  }

}
