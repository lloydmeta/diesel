package diesel.cats

import cats.arrow.FunctionK
import diesel.FunKLite

import scala.language.implicitConversions
import scala.language.higherKinds

package object implicits {

  implicit def functionKToFunKLite[F[_], G[_]](f: FunctionK[F, G]): FunKLite[F, G] =
    Conversions.FunctionKToFunKLit(f)

}
