package diesel

import scala.language.higherKinds

/**
  * Lite version of FunctionK (Cats) or NaturalTransformation (Scalaz).
  *
  * Conversions from either of those exist in diesel-cats or diesel-scalaz,
  *
  */
trait FunK[F[_], G[_]] {

  def apply[A](fa: F[A]): G[A]
}
