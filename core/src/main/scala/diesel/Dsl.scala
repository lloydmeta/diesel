package diesel

import scala.language.higherKinds

/**
  * A description of an operation that will be performed when given an interpreter.
  *
  * Note that nothing happens until the apply method is called with an interpreter.
  *
  * @tparam Alg An interpreter
  * @tparam A The value type wrapped by F[_]
  */
trait Dsl[Alg[_[_]], A] {

  /**
    * Evaluate this Dsl to a F[A]
    *
    * Provide an interpreter in order to evaluate to an F[A]
    *
    * @param interpreter of type Alg[F]
    * @tparam F a kind
    * @return F[A]
    */
  def apply[F[_]](implicit interpreter: Alg[F]): F[A]
}
