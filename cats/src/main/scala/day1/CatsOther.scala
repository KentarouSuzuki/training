package day1

import cats._
import implicits._

object CatsOther extends App{
  println(1 > 2.0)
  println(1 compare 2)
  println(1.5 max 1.6)

  println(1 tryCompare 2)

  def lt[A: PartialOrder](a1: A, a2: A): Boolean = a1 <= a2

  val res = lt[Int](1, 4)
  println(s"res: $res")
}
