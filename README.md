# Diesel

Boilerplate free Finally Tagless DSL annotation.

WIP 

## General idea

This plugin provides an annotation that expands a given trait into an object
holding a Tagless Final Algebra and DSL wrapper methods.

Example:

```scala
@diesel
trait Maths[F[_]] {
  def int(i: Int): F[Int]
  def add(l: F[Int], r: F[Int]): F[Int]
}

type Id[A] = A
val interpreter = new Maths.Algebra[Id] {
  def int(i: Int) = i
  def add(l: Id[Int], r: Id[Int]) = l + r
}

import Maths._

int(3)(interpreter) shouldBe 3
add(int(3), int(10))(interpreter) shouldBe 13

```

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