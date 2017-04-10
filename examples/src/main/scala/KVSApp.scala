import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSApp extends App {

  import KVStore._

  // This is one way to compose a program
  def program1[F[_]](implicit kvsAlg: KVSOps.Algebra[F]) = {
    import kvsAlg._
    import cats.implicits._
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int, Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n
  }

  // Another way (note that we don't need an implicit interpreter!) and the program is a value
  val program2 = {
    import KVSOps._
    import diesel.implicits.monadic._
    for {
      _ <- put("wild-cats", 90)
      _ <- update[Int, Int]("wild-cats", _ + 10)
      _ <- put("tame-cats", 55)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n
  }

  val r1 = program1[KVStoreState].run(Map.empty).value
  println(s"Result 1: $r1")
  val r2 = program2[KVStoreState].run(r1._1).value
  println(s"Result 2: $r2")

}
