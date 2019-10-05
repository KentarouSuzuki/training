package day14

import cats._
import cats.data._
import cats.implicits._

object CatsMonoidK extends App{
  val monoidKRes1 = Monoid[Option[Int]].empty
  println(monoidKRes1)

  val monoidKRes2 = MonoidK[Option].empty
  println(monoidKRes2)
}
