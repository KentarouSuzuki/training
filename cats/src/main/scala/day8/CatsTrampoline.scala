package day8

import cats._
import cats.data._
import cats.implicits._
import cats.free.{Free, Trampoline}
import Trampoline._

object CatsTrampoline extends App{
  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(true)
      case x :: xs => suspend(odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(false)
      case x :: xs => suspend(even(xs))
    }

  println(even(List(1, 2, 3, 4)).run)
  println(even((0 to 3000).toList).run)

  type FreeMonoid[A] = Free[(A, +?), Unit]
  def cons[A](a: A): FreeMonoid[A] =
    Free.liftF[(A, +?), Unit]((a, ()))

  println(cons(1))
  println(cons(1) flatMap {_ => cons(2)})

  implicit def tuple2Functor[A]: Functor[(A, ?)] =
    new Functor[(A, ?)] {
      def map[B, C](fa: (A, B))(f: B => C) = (fa._1, f(fa._2))
    }

  def toList[A](list: FreeMonoid[A]): List[A] = list.fold(
    {_ => Nil},
    {case (x: A @unchecked, xs: FreeMonoid[A]) => x :: toList(xs)}
  )

  println(toList(cons(1) flatMap{_ => cons(2) flatMap{_ => cons(3)}}))
}
