package day3

import cats._
import cats.data._
import cats.implicits._

object CatApplicative extends App{
  println(Applicative[List].pure(1))
  println(Applicative[Option].pure(1))

  val F = Applicative[Option]

  println(F.ap({F.pure((_: Int) + 3)})(F.pure(5)))

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil => Applicative[F].pure(Nil: List[A])
    case x :: xs => (x, sequenceA(xs)) mapN {_ :: _}
  }

  println(sequenceA(List(1.some, 3.some)))
  println(sequenceA(List(3.some, none[Int], 1.some)))
  println(sequenceA(List(List(1, 2, 3), List(4, 5, 6))))

  val f = sequenceA[Function[Int, ?], Int](List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1))
  println(f(3))
}
