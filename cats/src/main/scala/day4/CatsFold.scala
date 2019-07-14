package day4

import cats._
import cats.data._
import cats.implicits._

object CatsFold extends App{
  println(Foldable[List].foldLeft(List(1, 2, 3), 1){_ * _})
  println(Foldable[List].fold(List(1, 2, 3))(Monoid[Int]))

  val x = List(true, false, true).foldMap {Conjunction(_)}
  println(s"${x.unwrap}")
}
