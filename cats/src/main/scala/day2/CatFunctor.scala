package day2

import cats._
import implicits._

object CatFunctor extends App{
  val func = Functor[List].map(List(1, 2, 3)) {_ + 1}
  println(func)

  println((Right(1): Either[String, Int]).map{_ + 1})
  println((Left("boom!"): Either[String, Int]).map{_ + 1})

  val lifted = Functor[List].lift {(_: Int) * 2}
  println(lifted(List(1, 2, 3)))
  println(List(1, 2, 3).void)
  println(List(1, 2, 3) fproduct {(_: Int) * 2})
  println(List(1, 2, 3) as "x")

  val x: Either[String, Int] = Right(1)
  assert {(x map identity) === x}

  val f = {(_: Int) * 3}
  val g = {(_: Int) * 1}

  println(x map f map g)
}
