import cats.Monad

import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSLoggingApp extends App {

  import Maths.Ops._
  import Logger.Ops._
  import KVStore.Ops._, KVStore.KVStoreState
  import diesel.implicits.monadic._

  /**
    * Example of composing multiple languages.
    *
    * Here we have Logging and KVStore algebras mixed together and using a for-comprehension!
    */
  def program1[F[_]: Monad: KVStore: Logger] = {
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int, Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- info(n.toString)
      _ <- delete("tame-cats")
    } yield n
  }

  /**
    * Here we compose another DSL (MathOps) into our original composed program1
    */
  def program2[F[_]: Monad: KVStore: Logger: Maths] = {

    for {
      maybeX <- program1
      x = maybeX.getOrElse(1)
      maybeY <- program1
      y = maybeY.getOrElse(2)
      z <- add(int(x), int(y))
      _ <- put("wild-cats", z)
    } yield z
  }

  implicit val combinedInterp = new KVStore.KVSStateInterpreter with Logger.KVSStateInterpreter
  with Maths.KVSStateInterpreter {}

  val prog = for {
    r1 <- program1[KVStoreState]
    _ = println(s"Result 1: $r1")
    r2 <- program2[KVStoreState]
    _ = println(s"Result 2: $r2")
  } yield ()

  val _ = prog.run(Map.empty).value

}
