package diesel

import scala.language.higherKinds

trait LiteFunK [F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
