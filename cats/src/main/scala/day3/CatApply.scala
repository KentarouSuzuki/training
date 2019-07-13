package day3

import cats._
import cats.implicits._

object CatApply extends App{
  println((3.some, 5.some).mapN{_ - _})
  println((none[Int], 5.some).mapN{_ - _})
  println((3.some, none[Int]).mapN{_ - _})

  println(Apply[Option].ap({{(_: Int) + 3}.some})(9.some))
  println(Apply[Option].ap({{(_: Int) + 3}.some})(none[Int]))

  println((3.some, List(4).some) mapN {_ :: _})
  println(Apply[Option].ap2({{(_: Int) :: (_: List[Int])}.some})(3.some, List(4).some))
}
