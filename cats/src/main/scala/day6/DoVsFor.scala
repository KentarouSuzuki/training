package day6

import cats._
import cats.data._
import cats.implicits._

import collection.immutable.BitSet

object DoVsFor extends App{
  val bits = BitSet(1, 2, 3)
  val bitsFor = for {
    x <- bits
  } yield x.toFloat

  val listOptFor = for {
    i <- List(1, 2, 3)
    j <- Some(1)
  } yield i + j

  val mapOptFor = for {
    i <- Map(1 -> 2)
    j <- Some(3)
  } yield j

  println(bitsFor)
  println(listOptFor)
  println(mapOptFor)
}
