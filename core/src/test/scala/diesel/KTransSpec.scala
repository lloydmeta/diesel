package diesel

import cats.Monad
import cats.kernel.Monoid

class KTransSpec {

  @ktrans
  trait Trait1[F[_]] {
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]
    def paramss[A](k: String)(i: Int): F[Int]

    protected [diesel] def prot(p: Byte): F[Byte]
  }

  @ktrans
  trait TraitWithComp[F[_]] {
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]
    def paramss[A](k: String)(i: Int): F[Int]

    protected [diesel] def prot(p: Byte): F[Byte]
  }

  object TraitWithComp {
    val meh = 3
  }

  @ktrans
  abstract class AbsClass[G[_]] {
    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected [diesel] def prot(p: Byte): G[Byte]
  }

  @ktrans
  abstract class AbsClassWithCBound[G[_]: Monad] {
    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected [diesel] def prot(p: Byte): G[Byte]
  }

}
