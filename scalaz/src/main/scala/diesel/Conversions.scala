package diesel

import scalaz._
import scala.language.higherKinds

object Conversions {

  def natTransToFunKLite[F[_], G[_]](functionK: F ~> G) =
    new FunKLite[F, G] {
      def apply[A](fa: F[A]): G[A] = functionK(fa)
    }

}
