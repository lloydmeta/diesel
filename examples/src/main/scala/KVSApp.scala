import cats.Monad
import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSApp extends App {

  import KVStore._, Ops._

  import diesel.implicits.monadic._

  // This is one way to compose a program
  def program1[F[_]: Monad: KVStore]: F[Option[Int]] = {
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int, Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n
  }

  implicit object PureKVSInterp extends KVSStateInterpreter

  val prog = for {
    r1 <- program1[KVStoreState]
    _ = println(s"Result 1: $r1")
    r2 <- program1[KVStoreState]
    _ = println(s"Result 2: $r2")
  } yield ()

  val _ = prog.run(Map.empty).value

}
