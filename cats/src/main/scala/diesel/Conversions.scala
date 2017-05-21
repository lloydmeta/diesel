package diesel

import cats._
import scala.language.higherKinds

object Conversions {

  def funKToFunK[F[_], G[_]](functionK: F ~> G): FunK[F, G] = new FunK[F, G] {
    def apply[A](fa: F[A]): G[A] = functionK(fa)
  }

}
