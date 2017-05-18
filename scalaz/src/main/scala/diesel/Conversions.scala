package diesel

import scalaz._
import scala.language.higherKinds

object Conversions {

  def natTransToFunK[F[_], G[_]](functionK: F ~> G) =
    new FunK[F, G] {
      def apply[A](fa: F[A]): G[A] = functionK(fa)
    }

}
