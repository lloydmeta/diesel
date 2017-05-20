package diesel

import internal.Defaults

import scala.annotation.compileTimeOnly

/**
  * Annotation used for expanding a trait parameterised with a type that takes
  * one type application into a DSL.
  *
  * By default, the operation-wrapping method will be generated inside an object named "ops",
  * but this annotation * takes a String argument that you can use to customise it.
  *
  * There is also an `apply` method that can be bind an in-scope interpreter exactly like `implicitly`.
  *
  * Example:
  *
  * {{{
  * scala> import diesel._, cats._, cats.implicits._
  *
  * // Wrapper is only for the sake of sbt-doctest and is unnecessary in real-life usage
  * scala> object Wrapper {
  *      | // Declare our DSL
  *      | @diesel
  *      | trait Maths[G[_]] {
  *      |   def times(l: Int, r: Int): G[Int]
  *      |   def add(l: Int, r: Int): G[Int]
  *      | }
  *      | @diesel
  *      | trait Logger[F[_]] {
  *      |   def info(s: => String): F[Unit]
  *      | } }
  * scala> import Wrapper._
  *
  * // To use our DSL, import the converters that "alias" in-scope interpreters to their companion objects
  * scala> import Maths.ops._, Logger.ops._
  * scala> def loggedAdd[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
  *      |   for {
  *      |     s <- Maths.add(x, y)
  *      |     _ <- Logger.info(s"Sum was $s")
  *      |   } yield s
  *      | }
  *
  * scala> implicit val MathsIdInterp = new Maths[Id] {
  *      |   def times(l: Int, r: Int) = l * r
  *      |   def add(l: Int, r: Int)   = l + r
  *      | }
  *
  * scala> implicit val LoggerIdInterp = new Logger[Id] {
  *      |   def info(s: => String) = println(s)
  *      | }
  *
  * scala> loggedAdd[Id](3, 10)
  * res0: Int = 13
  * }}}
  */
@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
@compileTimeOnly("Enable macro paradise to expand macro annotations")
class diesel(dslName: String = Defaults.OpsName) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.DieselImpl.expand(this, defn)
//    println(r.syntax)
    r
  }

}