package day9

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

case class Prob[A](list: List[(A, Double)])

trait ProbInstances {
  def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
    def multall(innerxs: Prob[B], p: Double) = innerxs.list map {case (x, r) => (x, p * r)}
    Prob((xs.list flatMap {case (innerxs, p) => multall(innerxs, p)}))
  }

  implicit val probInstance: Monad[Prob] = new Monad[Prob] {
    override def pure[A](a: A): Prob[A] = Prob((a, 1.0) :: Nil)

    override def flatMap[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = this.flatten(map(fa)(f))
    override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] = Prob(fa.list map {case (x, p) => (f(x), p)})

    override def tailRecM[A, B](a: A)(f: A => Prob[Either[A, B]]): Prob[B] = {
      val buf = List.newBuilder[(B, Double)]
      @tailrec def go(lists: List[List[(Either[A, B], Double)]]): Unit =
        lists match {
          case (ab :: abs) :: tail => ab match {
            case (Right(b), p) =>
              buf += ((b, p))
              go(abs ::tail)

            case (Left(a), p) =>
              go(f(a).list :: abs :: tail)
          }
          case Nil :: tail => go(tail)
          case Nil => ()
        }
      go(f(a).list :: Nil)
      Prob(buf.result)
    }
  }
  implicit def probShow[A]: Show[Prob[A]] = Show.fromToString
}
case object Prob extends ProbInstances

sealed trait Coin
object Coin {
  case object Heads extends Coin
  case object Tails extends Coin
  implicit val coinEq: Eq[Coin] = new Eq[Coin] {
    override def eqv(x: Coin, y: Coin): Boolean = x == y
  }
  def heads: Coin = Heads
  def tails: Coin = Tails
}

object MakeMonad extends App{
  import Coin._
  def coin: Prob[Coin] = Prob(heads -> 0.5 :: tails -> 0.5 :: Nil)
  def loadedCoin: Prob[Coin] = Prob(heads -> 0.1 :: tails -> 0.9 :: Nil)

  def flipThree: Prob[Boolean] = for {
    a <- coin
    b <- coin
    c <- loadedCoin
  } yield {List(a, b, c) forall {_ === tails}}
  println(flipThree)

  println(Prob((3, 0.5) :: (5, 0.25) :: (9, 0.25) :: Nil) map {-_})
}
