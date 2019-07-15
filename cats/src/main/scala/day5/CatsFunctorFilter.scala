package day5

import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._

object CatsFunctorFilter extends App {
  val a1 = for {
    x <- (1 to 50).toList if x.show contains '7'
  } yield x
  println(a1)

  def collectEnglish[F[_]: FunctorEmpty](f: F[Int]): F[String] = f collect {
    case 1 => "one"
    case 3 => "three"
    case 10 => "ten"
  }
  println(collectEnglish((1 to 50).toList))

  def filterSeven[F[_]: FunctorEmpty](f: F[Int]): F[Int] = f filter {_.show contains "7"}
  println(filterSeven((1 to 50).toList))
}
