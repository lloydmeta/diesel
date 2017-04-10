import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSLoggingApp extends App {

  import Logger.LoggingOps
  import KVStore.{KVSOps, KVStoreState}

  /**
    * Example of composing multiple languages.
    *
    * Here we have Logging and KVStore algebras mixed together and using a for-comprehension!
    */
  def program1[F[_]](implicit kvsAlg: KVSOps.Algebra[F], loggingAlg: LoggingOps.Algebra[F]) = {
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

  type PRG[A[_]] = KVSOps.Algebra[A] with LoggingOps.Algebra[A]
  val program2 = {
    import KVSOps._
    import LoggingOps._
    import diesel.implicits.monadic._
    for {
      _ <- put("wild-cats", 90).withAlg[PRG]
      _ <- update[Int, Int]("wild-cats", _ + 10).withAlg[PRG]
      _ <- put("tame-cats", 55).withAlg[PRG]
      n <- get[Int]("wild-cats").withAlg[PRG]
      _ <- info(n.toString).withAlg[PRG]
      _ <- delete("tame-cats").withAlg[PRG]
    } yield n
  }

  implicit val combinedInterp = new KVStore.KVSStateInterpreter with Logger.KVSStateInterpreter {}

  val r1 = program1[KVStoreState].run(Map.empty).value
  println(s"Result 1: $r1")
  val r2 = program2[KVStoreState].run(r1._1).value
  println(s"Result 2: $r2")

}
