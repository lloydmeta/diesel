import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSApp extends App {

  import KVStore._

  def program[F[_]](implicit kvsAlg: KVSOps.Algebra[F]) = {
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

  val r = program[KVStoreState].run(Map.empty).value
  println(s"Result: $r")

}
