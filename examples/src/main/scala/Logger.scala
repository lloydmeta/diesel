import KVStore.KVStoreState
import cats.Monad
import diesel.diesel

@diesel
trait Logger[F[_]] {
  def info(s: String): F[Unit]
  def warn(s: String): F[Unit]
  def error(s: String): F[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Logger {

  implicit def loggingInterp[F[_]: Monad] = new Algebra[F] {
    private val m        = implicitly[Monad[F]]
    def info(s: String)  = m.pure(println(s"INFO: $s"))
    def warn(s: String)  = m.pure(println(s"WARN: $s"))
    def error(s: String) = m.pure(println(s"ERROR: $s"))
  }

  trait KVSStateInterpreter extends Algebra[KVStoreState] {
    private val m        = implicitly[Monad[KVStoreState]]
    def info(s: String)  = m.pure(println(s"INFO: $s"))
    def warn(s: String)  = m.pure(println(s"WARN: $s"))
    def error(s: String) = m.pure(println(s"ERROR: $s"))
  }

}