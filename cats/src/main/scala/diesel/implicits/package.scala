package diesel

import cats._

import scala.language.implicitConversions
import scala.language.higherKinds

package object implicits {

  implicit def functionKToFunKLite[F[_], G[_]](f: F ~> G): FunKLite[F, G] =
    Conversions.funKToFunKLite(f)

}
