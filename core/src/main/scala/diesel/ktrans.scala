package diesel

import scala.annotation.compileTimeOnly

/**
  * Annotation used for adding an extra method on to a given kind-parameterised trait that
  * allows you to easily transform the Kind.
  *
  * The method generated by default is named `transformK`, but you can customise it by
  * passing the annotation a literal string parameter.
  *
  * This saves us the trouble of having to write our own and forward each abstract method.
  *
  * Note, this only works if the kind is parameterised by only 1 type.
  *
  * Example:
  *
  * {{{
  * scala> import diesel._, cats._, cats.implicits._
  *
  * // Wrapper is only for the sake of sbt-doctest and is unnecessary in real-life usage
  * scala> object Wrapper {
  *      | // Declare our DSL
  *      | @ktrans
  *      | trait Maths[G[_]] {
  *      |   def add(l: Int, r: Int): G[Int]
  *      |   def subtract(l: Int, r: Int): G[Int]
  *      |   def times(l: Int, r: Int): G[Int]
  *      | }
  *      | // Example of customising the generated method's name
  *      | @ktrans("transK")
  *      | trait Logger[F[_]] {
  *      |   def info(s: => String): F[Unit]
  *      |   def warn(s: => String): F[Unit]
  *      |   def error(s: => String): F[Unit]
  *      | } }
  * scala> import Wrapper._
  *
  * scala> val MathsIdInterp = new Maths[Id] {
  *      |   def add(l: Int, r: Int) = l + r
  *      |   def subtract(l: Int, r: Int) = l - r
  *      |   def times(l: Int, r: Int) = l * r
  *      | }
  *
  * scala> val idToOpt = new FunKLite[Id, Option] {
  *      |   def apply[A](fa: Id[A]): Option[A] = Some(fa)
  *      | }
  *
  * scala> val MathsOptInterp = MathsIdInterp.transformK(idToOpt)
  *
  * scala> MathsOptInterp.add(3, 10)
  * res0: Option[Int] = Some(13)
  *
  * scala> val loggerIdInterp = new Logger[Id] {
  *      |   def info(s: => String) = println(s"Info: $s")
  *      |   def warn(s: => String) = println(s"Warn: $s")
  *      |   def error(s: => String)= println(s"Error: $s")
  *      | }
  * // Look ma, customised method name in action
  * scala> val loggerOptInterp: Logger[Option] = loggerIdInterp.transK(idToOpt)
  *
  * scala> loggerOptInterp.info("Yo")
  * res1: Option[Unit] = Some(())
  * }}}
  */
@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
@compileTimeOnly("Enable macro paradise to expand macro annotations")
class ktrans(transformMethodName: String = Defaults.transformMethodName) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.KTransImpl.expand(this, defn)
//  println(r.syntax)
    r
  }

}
