package day9

import cats._
import cats.data._
import cats.implicits._

object UnionMonadicFunc extends App{
  val f = Kleisli {x: Int => (x + 1).some}
  val g = Kleisli {x: Int => (x * 100).some}
  println(4.some >>= (f compose g).run)
  println(4.some >>= (f andThen g).run)

  val l = f.lift[List]
  println(List(1, 2, 3) >>= l.run)
}
