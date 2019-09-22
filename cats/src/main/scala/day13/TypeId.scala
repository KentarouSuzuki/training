package day13

import cats._
import cats.data._
import cats.implicits._

object TypeId extends App{
  val one: Id[Int] = 1

  val idFunctor = Functor[Id].map(one){_ + 1}
  println(idFunctor)

  val idApply = Apply[Id].ap({_ + 1}: Id[Int => Int])(one)
  println(idApply)

  val idFlatMap = FlatMap[Id].flatMap(one){_ + 1}
  println(idFlatMap)
}
