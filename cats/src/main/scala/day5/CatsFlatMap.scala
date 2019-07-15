package day5

import cats._
import cats.data._
import cats.implicits._

object CatsFlatMap extends App{
  println("wisdom".some map {_ + "!"})
  println(none[String] map {_ + "!"})

  println({(_: Int) + 3}.some ap 3.some)
  println(none[String => String] ap "greed".some)
  println({(_: String).toInt}.some ap none[String])

  println(3.some flatMap {x: Int => (x + 1).some})
  println("smile".some flatMap {x: String => (x + " :)").some})
  println(none[String] flatMap {x: String => (x + " :)").some})
}
