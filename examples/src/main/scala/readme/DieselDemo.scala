package readme

import diesel._, cats._, cats.implicits._

object DieselDemo {

  // Declare your DSL
  @diesel
  trait Maths[F[_]] {
    def times(l: Int, r: Int): F[Int]
    def add(l: Int, r: Int): F[Int]
  }

  @diesel
  trait Logger[F[_]] {
    def info(s: String): F[Unit]
  }

  // Import companion-to-interpreter aliasing sugar
  import Maths.ops._, Logger.ops._

  def prog[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
    for {
      p <- Maths.times(x, y)
      _ <- Logger.info(s"Product: $p")
      s <- Maths.add(x, y)
      _ <- Logger.info(s"Sum: $s")
      f <- Maths.add(p, s)
      _ <- Logger.info(s"Final: $s")
    } yield f
  }

  def main(args: Array[String]): Unit = {

    // Wire in our interpreters
    implicit val mathsInterp = new Maths[Id] {
      def times(l: Int, r: Int) = l * r
      def add(l: Int, r: Int)   = l + r
    }
    implicit val loggingInterp = new Logger[Id] {
      def info(msg: String) = println(msg)
    }

    val _ = prog[Id](3, 4)
  }

}
