package diesel

import scala.language.implicitConversions
import scala.language.higherKinds
import scalaz._

package object implicits {

  implicit def natTransformToFunK[F[_], G[_]](f: F ~> G): FunK[F, G] =
    Conversions.natTransToFunK(f)

}
