import cats.Monad
import diesel.{diesel, local}

import scala.language.higherKinds

object Logger {

  @diesel
  trait LoggingOps[F[_]] {

    @local
    implicit def m: Monad[F]

    def info(s: String): F[Unit]
    def warn(s: String): F[Unit]
    def error(s: String): F[Unit]

  }

  implicit def loggingInterp[F[_]: Monad] = new LoggingOps.Algebra[F] {
    val m                = implicitly[Monad[F]]
    def info(s: String)  = m.pure(println(s"INFO: $s"))
    def warn(s: String)  = m.pure(println(s"WARN: $s"))
    def error(s: String) = m.pure(println(s"ERROR: $s"))
  }

}
