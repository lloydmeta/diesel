# Diesel [![Build Status](https://travis-ci.org/lloydmeta/diesel.svg?branch=master)](https://travis-ci.org/lloydmeta/diesel) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11) [![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.15.svg)](https://www.scala-js.org)

Boilerplate free Tagless Final DSL macro annotation, written in [scala.meta](http://scalameta.org/) for future compatibility and other nice things (e.g. free IDE support, like in IntelliJ).

This library provides 2 macros:

  1. [`@diesel`](#diesel) to make it easier to compose algebras together.
  2. [`@ktrans`](#ktrans) to make it easier to perform Kind transforms on interpreters.
  
To use diesel in your project, [add it to your build](#sbt). 

## `@diesel`

The `@diesel` annotation cuts out the boilerplate associated with writing composable Tagless Final DSLs.

The Dsl can be accessed directly from the companion object if you import a converter located in `ops`
(customisable by passing a name to the annotation as an argument). This are useful when you need to compose multiple DSLs in the context of `F[_]`, but do not want to name all the interpreter parameters.

Note that this can be used along-side `@ktrans`.

### Example:

```scala
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
/*
[info] Running DieselDemo
Product: 12
Sum: 7
Final: 7
*/
```

For more in-depth examples, check out:

  1. [examples/KVSApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSApp.scala): a simple single-DSL program
  2. [examples/KVSLoggingApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSLoggingApp.scala): composing 2 DSLs in a program
  3. [examples/FibApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/FibApp.scala): composing 3 DSLs in a program that calculates fibonacci numbers and caches them.

All of the above examples use a pure KVS interpreter :)

### How it works

```scala
@diesel
trait Maths[F[_]] {
  def times(l: Int, r: Int): F[Int]
  def add(l: Int, r: Int): F[Int]
}
```

is expanded approximately into

```scala
// Your algebra. Implement by providing a concrete F and you have your interpreter
trait Maths[F[_]] {
    def times(l: Int, r: Int): F[Int]
    def add(l: Int, r: Int): F[Int]
}

// Helper methods will be added to the algebra's companion object (one will be created if there isn't one yet)
object Maths {

  def apply[F[_]](implicit m: Maths[F]): Maths[F] = m

  // In charge of aliasing your singleton Maths object to an in-scope Maths[F] :)
  object ops {
    implicit def toDsl[F[_]](o: Maths.type)(implicit m: Maths[F]): Maths[F] = m
  }
}

```


## `@ktrans`

The `@ktrans` annotation adds a `transformK` method to a trait that is parameterised by a Kind that takes 1 type parameter. 
It's handy when you want to transform any given implementation of that trait on `F[_]` into one that implements it on `G[_]`.

Useful because it saves you from having to write your boilerplate (method forwarding and results wrapping).
 
Note that this can be used along-side `@diesel`.

### Example

```scala
import diesel._, cats._
import diesel.implicits._

@ktrans
trait Maths[G[_]] {
  def add(l: Int, r: Int): G[Int]
  def subtract(l: Int, r: Int): G[Int]
  def times(l: Int, r: Int): G[Int]
}

val MathsIdInterp = new Maths[Id] {
  def add(l: Int, r: Int) = l + r
  def subtract(l: Int, r: Int) = l - r
  def times(l: Int, r: Int) = l * r
}

// Using kind-project syntax to build our natural transformation from
// Id to Option
val idToOpt =  λ[FunK[Id, Option]](Some(_))

// use the auto-generated transformK method to create a Maths[Option] from Maths[Id]
// via idToOpt
val MathsOptInterp = MathsIdInterp.transformK(idToOpt)

assert(MathsOptInterp.add(3, 10) == Some(13))
```

There are conversions from Cat's natural transformation (`FunctionK`) or Scalaz's `NaturalTransformation` in the
`diesel-cats` and `diesel-scalaz` companion projects.

### How it works

```scala
@ktrans
trait Maths[G[_]] {
  def add(l: Int, r: Int): G[Int]
  def subtract(l: Int, r: Int): G[Int]
  def times(l: Int, r: Int): G[Int]
}
```

is roughly expanded into

```scala
trait Maths[G[_]] {
  def add(l: Int, r: Int): G[Int]
  def subtract(l: Int, r: Int): G[Int]
  def times(l: Int, r: Int): G[Int]

  // Note that FunK is a really simple NaturalTransform / FunctionK
  final def transformK[H[_]](natTrans: FunK[G, H]): Maths[H] = {
    val curr = this
    new Maths[H] {
      def add(l: Int, r: Int): H[Int] = natTrans(curr.add(l, r))
      def subtract(l: Int, r: Int): H[Int] = natTrans(curr.subtract(l, r))
      def times(l: Int, r: Int): H[Int] = natTrans(curr.times(l, r))
    }
  }
}
```

### Limitations

  - Annottee must be parameterised by a higher kinded type with just 1 type parameter (context bounds allowed though)
  - No unimplemented methods that return types not contained by the kind parameter of the algebra
  - No unimplemented methods that take arguments parameterised by the same kind as the annottee
  - No unimplemented type members
  - No vals that are not assignments

## Sbt

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11)

Diesel is published for Scala 2.11, 2.12 and ScalaJS.

```scala
libraryDependencies += "com.beachape" %% "diesel-core" % s"$latest_version" % Compile

// Additional ceremony for using Scalameta macro annotations

resolvers += Resolver.url(
  "scalameta",
  url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)

// A dependency on macro paradise is required to both write and expand
// new-style macros.  This is similar to how it works for old-style macro
// annotations and a dependency on macro paradise 2.x.
addCompilerPlugin(
  "org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full)

scalacOptions += "-Xplugin-require:macroparadise"

```

# Credit

Learnt quite a lot about tagless final from the following resources.

1. [Free vs Tagless final talk](https://github.com/cb372/free-vs-tagless-final)
2. [Alternatives to GADTS in Scala](https://pchiusano.github.io/2014-05-20/scala-gadts.html)
3. [Quark talk](https://www.slideshare.net/jdegoes/quark-a-purelyfunctional-scala-dsl-for-data-processing-analytics)
4. [Tagless final effects à la Ermine writers](https://failex.blogspot.jp/2016/12/tagless-final-effects-la-ermine-writers.html)
5. [EDSLs as functions](http://typelevel.org/blog/2016/10/26/edsls-part-2.html)
