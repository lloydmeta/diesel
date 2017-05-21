package diesel

import cats.Id
import org.scalatest.{FunSpec, Matchers}

class KtransSpec extends FunSpec with Matchers {

  @ktrans
  trait Maths[G[_]] {
    def add(l: Int, r: Int): G[Int]
    def subtract(l: Int, r: Int): G[Int]
    def times(l: Int, r: Int): G[Int]
  }

  val MathsIdInterp = new Maths[Id] {
    def add(l: Int, r: Int)      = l + r
    def subtract(l: Int, r: Int) = l - r
    def times(l: Int, r: Int)    = l * r
  }

  val idToOpt = λ[FunK[Id, Option]](Some(_))

  // use the auto-generated transformK method to create a Maths[Option] from Maths[Id]
  val MathsOptInterp = MathsIdInterp.transformK(idToOpt)

  it("should transform properly") {

    MathsOptInterp.add(3, 10) shouldBe Some(13)
  }

}
