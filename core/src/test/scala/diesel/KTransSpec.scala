package diesel

import cats.kernel.Monoid

class KTransSpec {

  @ktrans
  trait Yo[F[_]] {
    def noArg: F[Int]
    def ser[A: Monoid](k: String, o: A): F[Unit]
  }

}
