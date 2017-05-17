# Diesel [![Build Status](https://travis-ci.org/lloydmeta/diesel.svg?branch=master)](https://travis-ci.org/lloydmeta/diesel) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11) [![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.15.svg)](https://www.scala-js.org)

Boilerplate free Tagless Final DSL macro annotation, written in [scala.meta](http://scalameta.org/) for future compatibility and other nice things (e.g. free IDE support, like in IntelliJ).

## General idea

This plugin provides an annotation that cuts out the boilerplate associated with writing composable Tagless 
Final DSLs.

The DSL wrapper methods are generated in the annotated trait's companion object, inside an object called `Dsl` 
(customisable by passing a name to the annotation as an argument). These are useful when you need to compose 
multiple DSLs in the context of `F[_]`.

Example:

```scala
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
/*
[info] Running DieselDemo 
result 3
*/
```

For more in-depth examples, check out:

  1. [examples/KVSApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSApp.scala): a simple single-DSL program 
  2. [examples/KVSLoggingApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSLoggingApp.scala): composing 2 DSLs in a program
  3. [examples/FibApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/FibApp.scala): composing 3 DSLs in a program that calculates fibonacci numbers and caches them.
  
All of the above examples use a pure KVS interpreter :)

## How it works

```scala
@diesel
trait Maths[F[_]] {
  def int(i: Int): F[Int]
  def add(l: F[Int], r: F[Int]): F[Int]
}
```

is expanded into

```scala
// Your algebra. Implement by providing a concrete F and you have your interpreter
trait Maths[F[_]] {
  def int(i: Int): F[Int]

  def add(l: F[Int], r: F[Int]): F[Int]
}

object Maths {

  import diesel.Dsl

  // Wrapper methods that allow you to delay deciding on a concrete F, and thus
  // are composable with other DSLs
  object Ops {
    def int(i: Int): Dsl[Maths, Int] = new Dsl[Maths, Int] {
      def apply[F[_]](implicit I: Maths[F]): F[Int] = I.int(i)
  }

    def add(l: Dsl[Maths, Int], r: Dsl[Maths, Int]): Dsl[Maths, Int] = new Dsl[Maths, Int] {
      def apply[F[_]](implicit I: Maths[F]): F[Int] = I.add(l.apply[F], r.apply[F])
    }
  }
}
```

## Sbt

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11)

```scala
libraryDependencies += "com.beachape" %% "diesel-core" % s"$latest_version"


// Additional ceremony for using Scalameta macro annotations

resolvers += Resolver.url(
  "scalameta",
  url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)

// A dependency on macro paradise is required to both write and expand
// new-style macros.  This is similar to how it works for old-style macro
// annotations and a dependency on macro paradise 2.x.
addCompilerPlugin(
  "org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full)

scalacOptions += "-Xplugin-require:macroparadise"

```


# Credit

Learnt quite a lot about tagless final from the following resources.

1. [Free vs Tagless final talk](https://github.com/cb372/free-vs-tagless-final)
2. [Alternatives to GADTS in Scala](https://pchiusano.github.io/2014-05-20/scala-gadts.html)
3. [Quark talk](https://www.slideshare.net/jdegoes/quark-a-purelyfunctional-scala-dsl-for-data-processing-analytics)
4. [Tagless final effects à la Ermine writers](https://failex.blogspot.jp/2016/12/tagless-final-effects-la-ermine-writers.html)
5. [EDSLs as functions](http://typelevel.org/blog/2016/10/26/edsls-part-2.html)
