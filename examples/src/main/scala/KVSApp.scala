import cats.Monad
import cats.implicits._
import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSApp extends App {

  import KVStore.ops._

  // This is one way to compose a program
  def program1[F[_]: Monad: KVStore]: F[Option[Int]] = {
    for {
      _ <- KVStore.put("wild-cats", 2)
      _ <- KVStore.update[Int, Int]("wild-cats", _ + 12)
      _ <- KVStore.put("tame-cats", 5)
      n <- KVStore.get[Int]("wild-cats")
      _ <- KVStore.delete("tame-cats")
    } yield n
  }

  import KVStore.KVStoreState

  val prog = for {
    r1 <- program1[KVStoreState]
    _ = println(s"Result 1: $r1")
    r2 <- program1[KVStoreState]
    _ = println(s"Result 2: $r2")
  } yield ()

  val _ = prog.run(Map.empty).value

}
