import KVStore.KVStoreState
import cats.Monad
import cats.implicits._

import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object FibApp extends App {

  /**
    * An example of composing 3 DSLs: Maths, Caching, Logging.
    *
    * Recursively calls itself as a bonus
    */
  def cachedFib[F[_]: Monad: KVStore: Maths: Logger](i: Int): F[Int] = {
    // Import the auto-generated methods
    import KVStore.Ops._, Logger.Ops._, Maths.Ops._

    i match {
      case _ if i <= 0 => Monad[F].pure(0)
      case 1           => Monad[F].pure(1)
      case _ => {
        for {
          fromCache <- get[Int](i.toString)[F]
          r <- fromCache match {
            case None =>
              for {
                _ <- warn(s"Not found in cache, trying to find fib of ${i - 1} and ${i - 2}")[F]
                a <- cachedFib[F](i - 1) /* Recursion! */
                b <- cachedFib[F](i - 2)
                s <- add(int(a), int(b))[F]
                _ <- info(s"Calculated fib of $i to be $s, caching")[F]
                _ <- put(i.toString, s)[F]
              } yield s
            case Some(f) =>
              for {
                _ <- info(s"Found fib of $i in cache: $f")[F]
              } yield f
          }
        } yield r
      }
    }

  }

  implicit val kvsStoreInterp = new KVStore.KVSStateInterpreter {}
  implicit val mathsInterp    = new Maths.KVSStateInterpreter   {}

  val prog = for {
    r1 <- cachedFib[KVStoreState](10)
    _ = println(r1)
    r2 <- cachedFib[KVStoreState](30)
    _ = println(r2)
    r3 <- cachedFib[KVStoreState](20)
  } yield ()

  val _ = prog.run(Map.empty).value

}
