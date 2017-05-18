package diesel

import scalaz.NaturalTransformation

object Conversions {

  def naturalTransformToFunKLit[F[_], G[_]](functionK: NaturalTransformation[F, G]) =
    new FunKLite[F, G] {
      def apply[A](fa: F[A]): G[A] = functionK(fa)
    }

}
