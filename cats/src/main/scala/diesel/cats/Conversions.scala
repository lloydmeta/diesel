package diesel.cats

import cats.arrow.FunctionK
import diesel.FunKLite

import scala.language.higherKinds

object Conversions {

  def FunctionKToFunKLit[F[_], G[_]](functionK: FunctionK[F, G]) = new FunKLite[F, G] {
    def apply[A](fa: F[A]): G[A] = functionK(fa)
  }

}
