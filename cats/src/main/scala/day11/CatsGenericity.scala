package day11

import cats._
import cats.data._
import cats.implicits._

trait Read[A] {
  def reads(s: String): Option[A]
}

object Read extends ReadInstances {
  def read[A](f: String => Option[A]): Read[A] = new Read[A]{
    override def reads(s: String): Option[A] = f(s)
  }
  def apply[A: Read]: Read[A] = implicitly[Read[A]]
}

trait ReadInstances {
  implicit val stringRead: Read[String] = Read.read[String]{Some(_)}
  implicit val intRead: Read[Int] = Read.read[Int] {s =>
    try {
      Some(s.toInt)
    } catch {
      case e : NumberFormatException => None
    }
  }
}

sealed trait Btree[A]
object Btree {
  case class Tip[A](a: A) extends Btree[A]
  case class Bin[A](left: Btree[A], right: Btree[A]) extends Btree[A]
}

object CatsGenericity extends App{
  def append[A](list: List[A], ys: List[A]): List[A] = list.foldLeft(ys){(acc, x) => x :: acc}
  println(append(List(1, 2, 3), List(4, 5, 6)))

  def sum(list: List[Int]): Int = list.foldLeft(0){_ + _}

  println(Read[Int].reads("1"))

  def foldB[A, B](tree: Btree[A], b: (B, B) => B)(t: A => B): B = tree match {
    case Btree.Tip(a) => t(a)
    case Btree.Bin(xs, ys) => b(foldB(xs, b)(t), foldB(ys, b)(t))
  }

}
