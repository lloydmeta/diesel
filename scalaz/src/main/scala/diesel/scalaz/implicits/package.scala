package diesel.scalaz

import diesel.FunKLite

import scala.language.implicitConversions
import scala.language.higherKinds
import scalaz.NaturalTransformation

package object implicits {

  implicit def natrualTransformToFunKLite[F[_], G[_]](
      f: NaturalTransformation[F, G]): FunKLite[F, G] =
    Conversions.naturalTransformToFunKLit(f)

}
