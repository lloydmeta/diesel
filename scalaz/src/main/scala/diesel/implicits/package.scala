package diesel

import scala.language.implicitConversions
import scala.language.higherKinds
import scalaz._

package object implicits {

  implicit def natTransformToFunKLite[F[_], G[_]](
      f: F ~> G): FunKLite[F, G] =
    Conversions.natTransToFunKLite(f)

}
