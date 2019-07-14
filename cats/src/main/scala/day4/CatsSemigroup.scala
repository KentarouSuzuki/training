package day4

import cats._
import cats.data._
import cats.implicits._

object CatsSemigroup extends App {
  assert{ (3 * 2) * (8 * 5) === 3 * (2 * (8 * 5)) }

  println(List(1, 2, 3) |+| List(4, 5, 6))
  println("one" |+| "two")

  def doSomething[A: Semigroup](a1: A, a2: A): A = a1 |+| a2

  println(doSomething(3, 5)(Semigroup[Int]))
}
