package day9

import cats._
import cats.data._
import cats.implicits._
import scala.util.Try

object RPNCalculator extends App{
  def foldingFunction(list: List[Double], next: String): List[Double] = (list, next) match {
    case (x :: y :: ys, "*") => (y * x) :: ys
    case (x :: y :: ys, "+") => (y + x) :: ys
    case (x :: y :: ys, "-") => (y - x) :: ys
    case (xs, numString) => numString.toInt :: xs
  }
  def solvePRN(s: String): Double = (s.split(' ').toList.foldLeft(Nil: List[Double]) {foldingFunction}).head
  println(solvePRN("10 4 3 + 2 * -"))

  def parseInt(x: String): Option[Int] =
    (Try(x.toInt) map { Some(_) }
      recover { case _: NumberFormatException => None}).get
  println(parseInt("1"))
  println(parseInt("foo"))

  def foldingFunction2(list: List[Double], next: String): List[Double] = (list, next) match {
    case (x :: y :: ys, "*") => (y * x) :: ys
    case (x :: y :: ys, "+") => (y + x) :: ys
    case (x :: y :: ys, "-") => (y + x) :: ys
    case (xs, numString) => numString.toInt :: xs
  }
  println(foldingFunction2(List(3, 2), "*"))
  println(foldingFunction2(Nil,"*"))
  println(foldingFunction2(Nil, "wawa"))
}
