import KVStore.KVStoreState
import cats.Monad
import diesel._

@diesel
trait Maths[F[_]] {
  def int(i: Int): F[Int]
  def add(x: F[Int], y: F[Int]): F[Int]
}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Maths {

  implicit object KVSStateInterpreter extends Maths[KVStoreState] {
    def int(i: Int) = Monad[KVStoreState].pure(i)
    def add(x: KVStoreState[Int], y: KVStoreState[Int]) =
      for {
        a <- x
        b <- y
      } yield a + b
  }

}
