package diesel

import scala.annotation.compileTimeOnly

/**
  * Annotation used for adding an extra method on to a given kind-parameterised trait that
  * allows you to easily transform the Kind.
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
  * }}}
  */
@compileTimeOnly("Enable macro paradise to expand macro annotations")
class ktrans extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.KTransImpl.expand(defn)
//  println(r.syntax)
    r
  }

}
