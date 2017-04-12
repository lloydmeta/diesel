import cats.Monad

import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSApp extends App {

  import KVStore._, Ops._
  import cats.implicits._

  // This is one way to compose a program
  def program1[F[_]: Monad: KVStore] = {
    for {
      _ <- put("wild-cats", 2)[F]
      _ <- update[Int, Int]("wild-cats", _ + 12)[F]
      _ <- put("tame-cats", 5)[F]
      n <- get[Int]("wild-cats")[F]
      _ <- delete("tame-cats")[F]
    } yield n
  }

  // Another way (note that we don't need an implicit interpreter!) and the program is a value
  val program2 = {
    import diesel.implicits.monadic._
    for {
      _ <- put("wild-cats", 90)
      _ <- update[Int, Int]("wild-cats", _ + 10)
      _ <- put("tame-cats", 55)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n
  }

  implicit object PureKVSInterp extends KVSStateInterpreter

  val r1 = program1[KVStoreState].run(Map.empty).value
  println(s"Result 1: $r1")
  val r2 = program2[KVStoreState].run(r1._1).value
  println(s"Result 2: $r2")

}
