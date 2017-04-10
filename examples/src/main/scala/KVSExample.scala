import cats.Monad
import cats.data.State
import diesel._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object KVSExample {

  @diesel
  trait KVS[F[_]] {

    @local
    implicit def m: Monad[F]

    def put[A](k: String, o: A): F[Unit]

    def get[A](k: String): F[Option[A]]

    def delete(k: String): F[Unit]

    def update[A, B](k: String, f: A => B): F[Unit] = {
      import cats.implicits._
      get[A](k).flatMap {
        case Some(v) => {
          val b = f(v)
          put(k, b)
        }
        case None => m.pure(())
      }
    }
  }

  type KVStoreState[A] = State[Map[String, Any], A]
  implicit val pureInterpreter = new KVS.Algebra[KVStoreState] {
    val m: Monad[KVStoreState] = Monad[KVStoreState]

    def put[A](k: String, o: A): KVStoreState[Unit] = State.modify(_.updated(k, o))

    def get[A](k: String): KVStoreState[Option[A]] = State.inspect(_.get(k).map(_.asInstanceOf[A]))

    def delete(k: String): KVStoreState[Unit] = State.modify(_ - k)
  }

  import implicits._
  import KVS._

  val program: KVStoreState[Option[Int]] = for {
    _ <- put("wild-cats", 2)
    _ <- update[Int, Int]("wild-cats", _ + 12)
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- delete("tame-cats")
  } yield n

  def main(args: Array[String]): Unit = {
    val r = program.run(Map.empty).value
    println(s"Result: $r")
  }

}
