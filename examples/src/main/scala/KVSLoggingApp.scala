import cats.Monad

import scala.language.higherKinds

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSLoggingApp extends App {

  import Maths._
  import Logger.info
  import KVStore._
  import cats.implicits._

  /**
    * Example of composing multiple languages.
    *
    * Here we have Logging and KVStore algebras mixed together and using a for-comprehension!
    */
  def program1[F[_]: Monad: KVStore.Algebra: Logger.Algebra] = {
    for {
      _ <- put("wild-cats", 2)[F]
      _ <- update[Int, Int]("wild-cats", _ + 12)[F]
      _ <- put("tame-cats", 5)[F]
      n <- get[Int]("wild-cats")[F]
      _ <- info(n.toString)[F]
      _ <- delete("tame-cats")[F]
    } yield n
  }

  /**
    * Here we compose another DSL (MathOps) into our original composed program1
    */
  def program2[F[_]: Monad: KVStore.Algebra: Logger.Algebra: Maths.Algebra] = {
    for {
      maybeX <- program1[F]
      x = maybeX.getOrElse(1)
      maybeY <- program1[F]
      y = maybeY.getOrElse(2)
      z <- add(int(x), int(y))[F]
      _ <- put("wild-cats", z)[F]
    } yield z
  }

  // Another way (note that we don't need an implicit interpreter!) and the program is a value
  type PRG[A[_]] = KVStore.Algebra[A] with Logger.Algebra[A]
  val program3 = {
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

  implicit val combinedInterp = new KVStore.KVSStateInterpreter with Logger.KVSStateInterpreter
  with Maths.KVSStateInterpreter {}

  val r1 = program1[KVStoreState].run(Map.empty).value
  println(s"Result 1: $r1")
  val r2 = program2[KVStoreState].run(r1._1).value
  println(s"Result 2: $r2")
  val r3 = program3[KVStoreState].run(r2._1).value
  println(s"Result 3: $r3")

}
