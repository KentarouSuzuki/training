package day1

import cats._
import implicits._

object CatsEq extends App{
  println(s"1 === 1: ${1 === 1}")
  println(s"foo === foo: ${"foo" === "foo"}")
}
