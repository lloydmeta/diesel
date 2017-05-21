package diesel

import cats._
import cats.kernel.Monoid

import scala.util.Try

object KtransAnnotationCompileTests {

  @ktrans
  @diesel
  trait Trait1[F[_]] {
    type Hey = Int
    val eh: Int = 3

    def simpleAbstDefMeth(yo: Int): Option[Either[Boolean, Try[Seq[Double]]]]
    val simpleAbstValMeth: Option[Either[Boolean, Seq[Try[Double]]]]

    def implementedDef(a: Int): Option[Boolean] = None

    val valThing, valThing2: F[Int]
    protected val protValThing: F[Option[Int]]
    protected[diesel] val packProtValThing: F[Option[Try[Either[String,Int]]]]
    private[diesel] val packPrivValThing: F[Option[Int]]

    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]

    def funct[G[_]: Monad, A](k: String): F[Option[A]]
    def funct2[G[_]: Monad, A](k: G[String], l: Option[G[A]]): F[Option[A]]

    def paramss[A](k: String)(i: Int): F[Int]

    protected[diesel] def packProt(p: Byte): F[Byte]
    protected def prot(p: Byte): F[Byte]

    private[diesel] def packPriv(p: Byte): F[Byte]

    // The following should fail compilation if uncommented
//    private def wut: F[Byte]
//    var eh = 9
//    def hmm(yo: F[Int]): Int
//    def hmmSuperNestedKParam(yo: Option[Either[Boolean, Try[Seq[F[Double]]]]]): Int
//    def hmmSuperNestedKRet(yo: Option[Either[Boolean, Try[Seq[Double]]]]): Option[Either[Boolean, Try[Seq[F[Double]]]]]
//    type Yo[A] = F[A]
//    protected def shadowGames[F[_]]: F[Int]


  }

  @diesel
  @ktrans
  trait TraitWithCompanion[F[_]] {
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]
    def paramss[A](k: String)(i: Int): F[Int]

    protected[diesel] def packProt(p: Byte): F[Byte]
    protected def prot(p: Byte): F[Byte]

    private[diesel] def packPriv(p: Byte): F[Byte]
  }

  object TraitWithCompanion {
    val meh = 3
  }

  @ktrans
  abstract class AbsClass[G[_]] {
    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected[diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private[diesel] def packPriv(p: Byte): G[Byte]
  }

  @ktrans
  abstract class AbsClassWithCBound[G[_]: Monad] { lulz =>
    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected[diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private[diesel] def packPriv(p: Byte): G[Byte]
  }

  @ktrans
  abstract class AbsClassWithCBoundAndParams[G[_]: Monad](age: Int /*, willBreakCompilation: G[Int]*/)(name: String, wut: Option[Int]) {
    lulz =>
    type Hey = Int
    val eh: Int = 3

    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected[diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private[diesel] def packPriv(p: Byte): G[Byte]
  }

}

@ktrans
trait Maths[G[_]] {
  def add(l: Int, r: Int): G[Int]
  def subtract(l: Int, r: Int): G[Int]
  def times(l: Int, r: Int): G[Int]
}
