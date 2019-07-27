package day9

import cats._
import cats.data._
import cats.implicits._

object MonadicFunction extends App{
  def join[F[_]: FlatMap, A](fa: F[F[A]]): F[A] = fa.flatten
  println(join(1.some.some))

  println(FlatMap[Option].flatten(1.some.some))

  def binSmalls(acc: Int, x: Int): Option[Int] = if (x > 9) none[Int] else (acc + x).some
  println((Foldable[List].foldM(List(2, 6, 3, 1), 0){binSmalls}))
  println(Foldable[List].foldM(List(2, 11, 3, 1), 0){binSmalls})
}
