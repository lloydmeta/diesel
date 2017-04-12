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

  /*
  [info] Running FibApp
WARN: Not found in cache, trying to find fib of 9 and 8
WARN: Not found in cache, trying to find fib of 8 and 7
WARN: Not found in cache, trying to find fib of 7 and 6
WARN: Not found in cache, trying to find fib of 6 and 5
WARN: Not found in cache, trying to find fib of 5 and 4
WARN: Not found in cache, trying to find fib of 4 and 3
WARN: Not found in cache, trying to find fib of 3 and 2
WARN: Not found in cache, trying to find fib of 2 and 1
WARN: Not found in cache, trying to find fib of 1 and 0
INFO: Calculated fib of 2 to be 1, caching
INFO: Calculated fib of 3 to be 2, caching
INFO: Found fib of 2 in cache: 1
INFO: Calculated fib of 4 to be 3, caching
INFO: Found fib of 3 in cache: 2
INFO: Calculated fib of 5 to be 5, caching
INFO: Found fib of 4 in cache: 3
INFO: Calculated fib of 6 to be 8, caching
INFO: Found fib of 5 in cache: 5
INFO: Calculated fib of 7 to be 13, caching
INFO: Found fib of 6 in cache: 8
INFO: Calculated fib of 8 to be 21, caching
INFO: Found fib of 7 in cache: 13
INFO: Calculated fib of 9 to be 34, caching
INFO: Found fib of 8 in cache: 21
INFO: Calculated fib of 10 to be 55, caching
55
WARN: Not found in cache, trying to find fib of 29 and 28
WARN: Not found in cache, trying to find fib of 28 and 27
WARN: Not found in cache, trying to find fib of 27 and 26
WARN: Not found in cache, trying to find fib of 26 and 25
WARN: Not found in cache, trying to find fib of 25 and 24
WARN: Not found in cache, trying to find fib of 24 and 23
WARN: Not found in cache, trying to find fib of 23 and 22
WARN: Not found in cache, trying to find fib of 22 and 21
WARN: Not found in cache, trying to find fib of 21 and 20
WARN: Not found in cache, trying to find fib of 20 and 19
WARN: Not found in cache, trying to find fib of 19 and 18
WARN: Not found in cache, trying to find fib of 18 and 17
WARN: Not found in cache, trying to find fib of 17 and 16
WARN: Not found in cache, trying to find fib of 16 and 15
WARN: Not found in cache, trying to find fib of 15 and 14
WARN: Not found in cache, trying to find fib of 14 and 13
WARN: Not found in cache, trying to find fib of 13 and 12
WARN: Not found in cache, trying to find fib of 12 and 11
WARN: Not found in cache, trying to find fib of 11 and 10
WARN: Not found in cache, trying to find fib of 10 and 9
INFO: Found fib of 10 in cache: 55
INFO: Found fib of 9 in cache: 34
INFO: Calculated fib of 11 to be 89, caching
INFO: Found fib of 10 in cache: 55
INFO: Calculated fib of 12 to be 144, caching
INFO: Found fib of 11 in cache: 89
INFO: Calculated fib of 13 to be 233, caching
INFO: Found fib of 12 in cache: 144
INFO: Calculated fib of 14 to be 377, caching
INFO: Found fib of 13 in cache: 233
INFO: Calculated fib of 15 to be 610, caching
INFO: Found fib of 14 in cache: 377
INFO: Calculated fib of 16 to be 987, caching
INFO: Found fib of 15 in cache: 610
INFO: Calculated fib of 17 to be 1597, caching
INFO: Found fib of 16 in cache: 987
INFO: Calculated fib of 18 to be 2584, caching
INFO: Found fib of 17 in cache: 1597
INFO: Calculated fib of 19 to be 4181, caching
INFO: Found fib of 18 in cache: 2584
INFO: Calculated fib of 20 to be 6765, caching
INFO: Found fib of 19 in cache: 4181
INFO: Calculated fib of 21 to be 10946, caching
INFO: Found fib of 20 in cache: 6765
INFO: Calculated fib of 22 to be 17711, caching
INFO: Found fib of 21 in cache: 10946
INFO: Calculated fib of 23 to be 28657, caching
INFO: Found fib of 22 in cache: 17711
INFO: Calculated fib of 24 to be 46368, caching
INFO: Found fib of 23 in cache: 28657
INFO: Calculated fib of 25 to be 75025, caching
INFO: Found fib of 24 in cache: 46368
INFO: Calculated fib of 26 to be 121393, caching
INFO: Found fib of 25 in cache: 75025
INFO: Calculated fib of 27 to be 196418, caching
INFO: Found fib of 26 in cache: 121393
INFO: Calculated fib of 28 to be 317811, caching
INFO: Found fib of 27 in cache: 196418
INFO: Calculated fib of 29 to be 514229, caching
INFO: Found fib of 28 in cache: 317811
INFO: Calculated fib of 30 to be 832040, caching
832040
INFO: Found fib of 20 in cache: 6765

 */
}
