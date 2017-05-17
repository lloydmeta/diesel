import cats.Monad
import cats.implicits._
import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSLoggingApp extends App {

  import Maths.Dsl._
  import Logger.Dsl._
  import KVStore.Dsl._

  /**
    * Example of composing multiple languages.
    *
    * Here we have Logging and KVStore algebras mixed together and using a for-comprehension!
    */
  def program1[F[_]: Monad: KVStore: Logger] = {
    for {
      _ <- KVStore.put("wild-cats", 2)
      _ <- KVStore.update[Int, Int]("wild-cats", _ + 12)
      _ <- KVStore.put("tame-cats", 5)
      n <- KVStore.get[Int]("wild-cats")
      _ <- Logger.info(n.toString)
      _ <- KVStore.delete("tame-cats")
    } yield n
  }

  /**
    * Here we compose another DSL (MathOps) into our original composed program1
    */
  def program2[F[_]: Monad: KVStore: Logger: Maths] = {

    for {
      maybeX <- program1[F]
      x = maybeX.getOrElse(1)
      maybeY <- program1[F]
      y = maybeY.getOrElse(2)
      z <- Maths.add(Maths.int(x), Maths.int(y))
      _ <- KVStore.put("wild-cats", z)
    } yield z
  }

  import KVStore.KVStoreState
  val prog = for {
    r1 <- program1[KVStoreState]
    _ = println(s"Result 1: $r1")
    r2 <- program2[KVStoreState]
    _ = println(s"Result 2: $r2")
  } yield ()

  val _ = prog.run(Map.empty).value

}
