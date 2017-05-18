package diesel

import cats.Monad
import cats.kernel.Monoid

class KTransSpec {

  @ktrans
  trait Trait1[F[_]] {
    val vlaThing, vlaThing2: F[Int]
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]
    def paramss[A](k: String)(i: Int): F[Int]

    protected [diesel] def packProt(p: Byte): F[Byte]
    protected def prot(p: Byte): F[Byte]

    private [diesel] def packPriv(p: Byte): F[Byte]
  }

  @ktrans
  trait TraitWithComp[F[_]] {
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
    def des[A: Monoid](k: String): F[Option[A]]
    def paramss[A](k: String)(i: Int): F[Int]

    protected [diesel] def packProt(p: Byte): F[Byte]
    protected def prot(p: Byte): F[Byte]

    private [diesel] def packPriv(p: Byte): F[Byte]
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

    protected [diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private [diesel] def packPriv(p: Byte): G[Byte]
  }

  @ktrans
  abstract class AbsClassWithCBound[G[_]: Monad] { lulz =>
    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected [diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private [diesel] def packPriv(p: Byte): G[Byte]
  }

  @ktrans
  abstract class AbsClassWithCBoundAndParams[G[_]: Monad](age: Int)(name: String) { lulz =>
    type Hey = Int
    val eh: Int = 3

    def noArg: G[Int]
    def ser[A: Monoid](k: String, o: A): G[Unit]
    def des[A: Monoid](k: String): G[Option[A]]
    def paramss[A](k: String)(i: Int): G[Int]

    protected [diesel] def packProt(p: Byte): G[Byte]
    protected def prot(p: Byte): G[Byte]

    private [diesel] def packPriv(p: Byte): G[Byte]
  }


}
