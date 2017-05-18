package diesel

import scala.language.higherKinds

trait FunKLite [F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
