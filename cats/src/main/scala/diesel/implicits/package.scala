package diesel

import cats.{Monad, MonadFilter}

/**
  * Allows the use of DSL-wrapped things in for-comprehensions
  */
package object implicits {

  implicit def toM[Alg[_[_]], A, M[_]](dsl: Dsl[Alg, A])(implicit interp: Alg[M]): M[A] = dsl[M]

  implicit class ForComprehensionOps[Alg[_[_]], A, M[_]](
      dsl: Dsl[Alg, A])(implicit interp: Alg[M], M: Monad[M], MF: MonadFilter[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.flatMap(dsl)(f)

    def map[B](f: A => B): M[B] = M.map(dsl)(f)

    def filter(f: A => Boolean): M[A] = MF.filter(dsl)(f)

    def withFilter(f: A => Boolean): M[A] = filter(f)
  }

}
