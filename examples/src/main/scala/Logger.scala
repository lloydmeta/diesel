import KVStore.KVStoreState
import cats.Monad
import diesel.diesel

import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Logger {

  @diesel
  trait LoggingOps[F[_]] {
    def info(s: String): F[Unit]
    def warn(s: String): F[Unit]
    def error(s: String): F[Unit]

  }

  implicit def loggingInterp[F[_]: Monad] = new LoggingOps.Algebra[F] {
    private val m        = implicitly[Monad[F]]
    def info(s: String)  = m.pure(println(s"INFO: $s"))
    def warn(s: String)  = m.pure(println(s"WARN: $s"))
    def error(s: String) = m.pure(println(s"ERROR: $s"))
  }

  trait KVSStateInterpreter extends LoggingOps.Algebra[KVStoreState] {
    private val m        = implicitly[Monad[KVStoreState]]
    def info(s: String)  = m.pure(println(s"INFO: $s"))
    def warn(s: String)  = m.pure(println(s"WARN: $s"))
    def error(s: String) = m.pure(println(s"ERROR: $s"))
  }

}
