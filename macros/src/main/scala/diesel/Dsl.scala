package diesel

import scala.language.higherKinds

/**
  * A description of an operation that will be performed when given an interpreter
  *
  * @tparam Alg An interpreter
  * @tparam A The value type wrapped by F[_]
  */
trait Dsl[Alg[_[_]], A] {

  /**
    * Provide an interpreter in order to evaluate to an F[A]
    * @param interpreter of type Alg[F]
    * @tparam F a kind
    * @return F[A]
    */
  def apply[F[_]](implicit interpreter: Alg[F]): F[A]
}
