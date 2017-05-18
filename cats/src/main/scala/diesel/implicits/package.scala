package diesel

import cats._

import scala.language.implicitConversions
import scala.language.higherKinds

package object implicits {

  implicit def functionKToFunK[F[_], G[_]](f: F ~> G): FunK[F, G] =
    Conversions.funKToFunK(f)

}
