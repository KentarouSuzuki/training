package day12

import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.duration._
import scala.concurrent.{Future, Await, ExecutionContext}

object CatsTraverse extends App{
  val ans1 = List(1, 2, 3).traverse[Id, Int]{x: Int => x + 1}
  val ans2 = List(1, 2, 3).traverse{x: Int => Some(x + 1): Option[Int]}
  val ans3 = List(1, 2, 3).traverse{x: Int => None}
  println(s"$ans1\n$ans2\n$ans3")

  def reduce[A, B, F[_]](fa: F[A])(f: A => B)(implicit FF: Traverse[F], BB: Monoid[B]): B = {
    val g: A => Const[B, Unit] = {a: A => Const(f(a))}
    val x = FF.traverse[Const[B, ?], A, Unit](fa)(g)
    x.getConst
  }
  val ans4 = reduce(List('a', 'b', 'c')){c: Char => c.toInt}
  println(ans4)

  def reduce2[A, B, F[_]](fa: F[A])(f: A => B)(implicit FF: Traverse[F], BB: Monoid[B]): B = {
    val x = fa.traverse{ a: A => Const[B, Unit](f(a))}
    x.getConst
  }
  val ans5 = reduce2(List('a', 'b', 'c')){c: Char => c.toInt}
  println(ans5)

  val x = {
    implicit val ec = ExecutionContext.global
    List(Future(1), Future(2), Future(3)).sequence
  }
  val ans6 = Await.result(x, 1 second)
  println(ans6)

  val ans7 = List(Right(1): Either[String, Int]).sequence
  val ans8 = List(Right(1): Either[String, Int], Left("boom"): Either[String, Int]).sequence
  println(s"$ans7\n$ans8")
}
