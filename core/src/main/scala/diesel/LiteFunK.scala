package diesel

trait LiteFunK [F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
