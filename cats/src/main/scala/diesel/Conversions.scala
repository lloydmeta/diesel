package diesel

import cats._
import scala.language.higherKinds

object Conversions {

  def funKToFunKLite[F[_], G[_]](functionK: F ~> G) = new FunKLite[F, G] {
    def apply[A](fa: F[A]): G[A] = functionK(fa)
  }

}
