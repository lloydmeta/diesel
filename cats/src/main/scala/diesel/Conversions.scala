package diesel

import cats.arrow.FunctionK

object Conversions {

  def FunctionKToFunKLit[F[_], G[_]](functionK: FunctionK[F, G]) = new FunKLite[F, G] {
    def apply[A](fa: F[A]): G[A] = functionK(fa)
  }

}
