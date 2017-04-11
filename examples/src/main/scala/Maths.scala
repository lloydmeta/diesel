import KVStore.KVStoreState
import cats.Monad
import diesel._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Maths {

  @diesel
  trait MathOps[F[_]] {
    def int(i: Int): F[Int]
    def add(x: F[Int], y: F[Int]): F[Int]
  }

  trait KVSStateInterpreter extends MathOps.Algebra[KVStoreState] {
    private val m   = implicitly[Monad[KVStoreState]]
    def int(i: Int) = m.pure(i)
    def add(x: KVStoreState[Int], y: KVStoreState[Int]) =
      for {
        a <- x
        b <- y
      } yield a + b
  }

}
