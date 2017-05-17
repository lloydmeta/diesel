package readme

import diesel._, cats._, cats.implicits._

object DieselDemo  {

  // Declare your DSL
  @diesel
  trait Maths[F[_]] {
    def int(i: Int): F[Int]
    def add(l: F[Int], r: F[Int]): F[Int]
  }

  @diesel
  trait Logger[F[_]] {
    def info(s: String): F[Unit]
  }

  // Use the Dsl wrapping methods
  import Maths.Dsl._, Logger.Dsl._
  def addAndLog[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
    for {
      r <- Maths.add(Maths.int(x), Maths.int(y))
      _ <- Logger.info(s"result $r")
    } yield r
  }

  def main(args: Array[String]): Unit = {

    // Wire in our interpreters
    implicit val mathsInterp = new Maths[Id] {
      def int(a: Int)                 = a
      def add(a: Id[Int], b: Id[Int]) = a + b
    }
    implicit val loggingInterp = new Logger[Id] {
      def info(msg: String)           = println(msg)
    }

    addAndLog[Id](1, 2)
    ()
  }

}