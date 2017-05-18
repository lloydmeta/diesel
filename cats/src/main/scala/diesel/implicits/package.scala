package diesel

import cats.arrow.FunctionK

import scala.language.implicitConversions
import scala.language.higherKinds

package object implicits {

  implicit def functionKToFunKLite[F[_], G[_]](f: FunctionK[F, G]): FunKLite[F, G] =
    Conversions.FunctionKToFunKLit(f)

}
