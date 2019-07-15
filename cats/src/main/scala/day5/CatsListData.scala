package day5

import cats._
import cats.data._
import cats.implicits._

object CatsListData extends App{
  println((List(1, 2, 3), List(4, 5, 6)) mapN {_ * _})
  println(List(3, 4, 5) >>= {x => List(x, -x)})
}
