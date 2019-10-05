package day14

import cats._
import cats.data._
import cats.implicits._

case class Foo(x: String)

object CatsSemigroupK extends App {
  val semigroupRes1 = List(1, 2, 3) |+| List(4, 5, 6)
  println(semigroupRes1)

  val semigroupRes2 = "one" |+| "tow"
  println(semigroupRes2)

  val semigroupKRes1 = List(1, 2, 3) <+> List(4, 5, 6)
  println(semigroupKRes1)

  val semigroupKRes2 = Foo("x").some <+> Foo("y").some
  println(semigroupKRes2)

  println(1.some |+| 2.some)
  println(1.some <+> 2.some)
}
