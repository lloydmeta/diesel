import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSLoggingApp {

  import Logger._
  import KVStore._

  /**
    * Example of composing multiple languages.
    *
    * Here we have Logging and KVStore algebras mixed together and using a for-comprehension!
    */
  def program[F[_]](implicit kvsAlg: KVSOps.Algebra[F], loggingAlg: LoggingOps.Algebra[F]) = {
    import kvsAlg._
    import cats.implicits._
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int, Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- loggingAlg.info(n.toString)
      _ <- delete("tame-cats")
    } yield n
  }

  def main(args: Array[String]): Unit = {
    val r = program[KVStoreState].run(Map.empty).value
    println(s"Result: $r")
  }

}
