package day12

import cats._
import cats.data._
import cats.implicits._

import cats.data.Func.appFunc

object TypeContents extends App {
  def contents[F[_], A](fa: F[A])(implicit FF: Traverse[F]): Const[List[A], F[Unit]] = {
    val contentsBody: A => Const[List[A], Unit] = (a: A) => Const(List(a))
    FF.traverse(fa)(contentsBody)
  }
  val ans1 = contents(Vector(1, 2, 3)).getConst
  println(ans1)

  def shape[F[_], A](fa: F[A])(implicit FF: Traverse[F]): Id[F[Unit]] = {
    val shapeBody: A => Id[Unit] = (a: A) => ()
    FF.traverse(fa)(shapeBody)
  }
  val ans2 = shape(Vector(1, 2, 3))
  println(ans2)

  def decompose[F[_], A](fa: F[A])(implicit FF: Traverse[F]) =
    Tuple2K[Const[List[A], ?], Id, F[Unit]](contents(fa), shape(fa))
  val d = decompose(Vector(1, 2, 3))
  println(d.first)
  println(d.second)

  def contentsBody[A] = appFunc[Const[List[A], ?], A, Unit]{a: A => Const(List(a))}
  def shapeBody[A] = appFunc{(a: A) => ((): Id[Unit])}
  def decompose2[F[_], A](fa: F[A])(implicit FF: Traverse[F]) = (contentsBody[A] product shapeBody[A]).traverse(fa)

  val d2 = decompose(Vector(1, 2, 3))
  println(d2.first)
  println(d2.second)
}
