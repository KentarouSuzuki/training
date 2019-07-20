package day7

import cats._
import cats.data._
import cats.implicits._
import Validated.{valid, invalid}

object CatsValidated extends App{
  val result = (
    valid[String, String]("event 1 ok") |@|
    invalid[String, String]("event 2 failed!") |@|
    invalid[String, String]("event 3 failed!")
    ).map {_ + _ + _}
  println(result)

  val result2 = (
    valid[NonEmptyList[String], String]("event 1 ok") |@|
    invalid[NonEmptyList[String], String](NonEmptyList.of("event 2 failed!")) |@|
    invalid[NonEmptyList[String], String](NonEmptyList.of("event 3 failed!"))
  ) map {_ + _ + _}
  println(
    result2.fold({l => l}, {_ => sys.error("invalid is expected")})
  )
}
