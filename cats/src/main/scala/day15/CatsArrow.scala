package day15

import cats._
import cats.data._
import cats.implicits._

object CatsArrow extends App{
  val f = (_: Int) + 1
  val g = (_: Int) * 100

  println((f >>> g)(2))
  println((f <<< g)(2))

  val f_first = f.first[Int]
  println(f_first((1, 1)))

  val f_second = f.second[Int]
  println(f_second((1, 1)))

  val fgSplit = (f split g)((1, 1))
  println(fgSplit)
}
