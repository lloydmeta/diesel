package diesel

import scala.annotation.compileTimeOnly

/**
  * Annotation used for expanding a trait parameterised with a type that takes
  * one type application into a DSL.
  *
  * By default, the operations generated will have the name "Ops", but this annotation
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
  * scala> import diesel._
  *
  * // Wrapper is only for the sake of sbt-doctest and is unnecessary in real-life usage
  * scala> object Wrapper {
  *      | // Declare our DSL
  *      | @diesel
  *      | trait Maths[G[_]] {
  *      |   def int(i: Int): G[Int]
  *      |   def add(l: G[Int], r: G[Int]): G[Int]
  *      | } }
  * scala> import Wrapper._
  *
  * // Write an interpreter
  * scala> type Id[A] = A
  * scala> val interpreter = new Maths[Id] {
  *      |   def int(i: Int)                 = i
  *      |   def add(l: Id[Int], r: Id[Int]) = l + r
  *      | }
  *
  * // Now we can use our DSL
  * scala> import Maths._, Ops._
  *
  * scala> int(3)(interpreter)
  * res0: Int = 3
  *
  * scala> add(int(3), int(10))(interpreter)
  * res1: Int = 13
  * }}}
  */
@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
@compileTimeOnly("Enable macro paradise to expand macro annotations")
class diesel(opsName: String = Defaults.OpsName) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.MacroImpl.expand(this, defn)
//    println(r.syntax)
    r
  }

}

object Defaults {
  val OpsName: String = "Ops"
}

/**
  * Used to denote declarations inside a @diesel-annotated trait so that it will
  * be forwarded as-is into the generated Algebra trait, without generating any DSL-wrapper
  * methods
  */
class local extends scala.annotation.StaticAnnotation