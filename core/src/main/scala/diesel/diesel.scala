package diesel

import scala.annotation.compileTimeOnly

/**
  * Annotation used for expanding a trait parameterised with a type that takes
  * one type application into a DSL.
  *
  * By default, the operations generated will have the name "Dsl", but this annotation
  * takes a String argument that you can use to customise it.
  *
  * If you wish to put concrete methods into the resulting companion object, write them
  * in a separate concrete object. The Tagless-Final expansions will be prepended to the
  * body of the companion object that you write. If you don't write a companion object,
  * one will be created.
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
  *      |   def add(l: Int, r: Int): G[Int]
  *      | }
  *      | @diesel
  *      | trait Logger[F[_]] {
  *      |   def info(s: => String): F[Unit]
  *      | } }
  * scala> import Wrapper._
  *
  * // To use our DSL, use the magic imports that "alias" in-scope interpreters to their companion objects
  * scala> import Maths.Dsl._, Logger.Dsl._
  *
  * scala> def loggedAdd[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
  *      |   for {
  *      |     s <- Maths.add(x, y)
  *      |     _ <- Logger.info(s"Sum was $s")
  *      |   } yield s
  *      | }
  *
  * scala> implicit val MathsIdInterp = new Maths[Id] {
  *      |   def add(l: Int, r: Int) = l + r
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
class diesel(dslName: String = Defaults.DslName) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.MacroImpl.expand(this, defn)
//    println(r.syntax)
    r
  }

}

object Defaults {
  val DslName: String = "Dsl"
}
