package day1

import cats._
import implicits._

object CatsShow extends App{
  println(3.show)
  println("hello".show)

  case class Person(name: String)
  case class Car(model: String)

  implicit val personShow = Show.show[Person](p => p.name)
  implicit val carShow = Show.fromToString[Car]

  println(Person("Tom").show)
  println(Car("Prius").show)
}
