package day2

import simulacrum._

@typeclass trait CanTruthy[A] {self =>
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    override def truthy(a: A): Boolean = f(a)
  }
}

@typeclass trait CanAppend[A] {
  @op("|+|") def append(a1: A, a2: A): A
}

object Truthy extends App{
  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 => false
    case _ => true
  })
  import CanTruthy.ops._
  println(10.truthy)

  implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
    override def append(a1: Int, a2: Int): Int = a1 + a2
  }
  import CanAppend.ops._
  println(1 |+| 2)
}
