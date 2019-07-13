package day3

import cats._
import cats.data._
import cats.implicits._

object Semigroupal extends App {
  val hs = Functor[List].map(List(1, 2, 3, 4))({(_: Int) * (_ : Int)}.curried)
  println(Functor[List].map(hs){_(9)})
}
