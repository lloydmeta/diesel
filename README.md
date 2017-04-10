# Diesel [![Build Status](https://travis-ci.org/lloydmeta/diesel.svg?branch=master)](https://travis-ci.org/lloydmeta/diesel) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.beachape/diesel-core_2.11)

Boilerplate free Finally Tagless DSL macro annotation, written in [scala.meta](http://scalameta.org/) for future compatibility and other nice things (like free IDE support).

WIP

## General idea

This plugin provides an annotation that expands a given trait into an object
holding a Tagless Final Algebra and DSL wrapper methods.

Example:

```scala
// Declare your DSL
@diesel
trait Maths[F[_]] {
  def int(i: Int): F[Int]
  def add(l: F[Int], r: F[Int]): F[Int]
}

// Write your interpreter
type Id[A] = A
val interpreter = new Maths.Algebra[Id] {
  def int(i: Int) = i
  def add(l: Id[Int], r: Id[Int]) = l + r
}

// Use your stuff.
import Maths._
int(3)(interpreter) shouldBe 3
add(int(3), int(10))(interpreter) shouldBe 13
```

For more in-depth examples, check out:

  1. [examples/KVSApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSApp.scala): a simple single-DSL program 
  2. [examples/KVSLoggingApp](https://github.com/lloydmeta/diesel/blob/master/examples/src/main/scala/KVSLoggingApp.scala): mixing 2 DSLs in a program

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
object Maths {

  import scala.language.higherKinds
  import _root_.diesel.Dsl

  trait Algebra[F[_]] {
    def int(i: Int): F[Int]

    def add(l: F[Int], r: F[Int]): F[Int]
  }

  def int(i: Int): Dsl[Algebra, Int] = new Dsl[Algebra, Int] {
    def apply[F[_]](implicit interpreter: Algebra[F]): F[Int] = interpreter.int(i)
  }

  def add(l: Dsl[Algebra, Int], r: Dsl[Algebra, Int]): Dsl[Algebra, Int] = new Dsl[Algebra, Int] {
    def apply[F[_]](implicit interpreter: Algebra[F]): F[Int] = interpreter.add(l.apply[F], r.apply[F])
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

There are also 2 sub-projects that provide implicit conversions from Dsl to a Monad (in the presence of an interpreter)
so that you can use your DSL with for-comprehensions as demonstrated in @cb372's talk below.

Choose one or the other:

```scala
// for cats
libraryDependencies += "com.beachape" %% "diesel-cats" % s"$latest_version"
// for scalaz  
libraryDependencies += "com.beachape" %% "diesel-scalaz" % s"$latest_version"
```

# Credit
1. [Free vs Tagless final talk](https://github.com/cb372/free-vs-tagless-final)
2. [Alternatives to GADTS in Scala](https://pchiusano.github.io/2014-05-20/scala-gadts.html)
3. [Quark talk](https://www.slideshare.net/jdegoes/quark-a-purelyfunctional-scala-dsl-for-data-processing-analytics)
