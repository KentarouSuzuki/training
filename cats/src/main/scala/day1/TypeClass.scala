package day1

import cats._

sealed trait TrafficLight
object TrafficLight {
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  def red: TrafficLight = Red
  def yellow: TrafficLight = Yellow
  def green: TrafficLight = Green

  implicit val trafficLightEq: Eq[TrafficLight] = new Eq[TrafficLight]{
    def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
  }
}

object TypeClass extends App{
  import TrafficLight._

  println(red == red)
  println(green == yellow)
}
