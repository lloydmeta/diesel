import cats.Monad
import cats.data.State
import diesel.{diesel, local}

@diesel
trait KVStore[F[_]] {

  @local
  implicit def m: Monad[F]

  def put[A](k: String, o: A): F[Unit]

  def get[A](k: String): F[Option[A]]

  def delete(k: String): F[Unit]

  def update[A, B](k: String, f: A => B): F[Unit] = {
    import cats.implicits._
    get[A](k).flatMap {
      case Some(v) => {
        val b = f(v)
        put(k, b)
      }
      case None => m.pure(())
    }
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVStore {

  type KVStoreState[A] = State[Map[String, Any], A]

  trait KVSStateInterpreter extends KVStore[KVStoreState] {
    val m: Monad[KVStoreState] = Monad[KVStoreState]

    def put[A](k: String, o: A): KVStoreState[Unit] = State.modify(_.updated(k, o))

    def get[A](k: String): KVStoreState[Option[A]] = State.inspect(_.get(k).map(_.asInstanceOf[A]))

    def delete(k: String): KVStoreState[Unit] = State.modify(_ - k)
  }

}