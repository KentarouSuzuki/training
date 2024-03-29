package day14

import cats._
import cats.data._
import cats.implicits._

sealed trait Character
case object Farmer extends Character
case object Wolf extends Character
case object Goat extends Character
case object Cabbage extends Character

case class Move(x: Character)
case class Plan(moves: List[Move])

sealed trait Position
case object West extends Position
case object East extends Position

object CatsAlternative extends App{
  implicit val moveShow = Show.show[Move] {
    case Move(Farmer) => "F"
    case Move(Wolf) => "W"
    case Move(Goat) => "G"
    case Move(Cabbage) => "C"
  }

  val possibleMoves = List(Farmer, Goat, Cabbage, Goat, Cabbage) map {Move(_)}
  def makeMove(ps: List[List[Move]]): List[List[Move]] = (ps |@| possibleMoves) map { (p, m) => List(m) <+> p}

  def makeNMove(n: Int): List[List[Move]] = n match {
    case 0 => Nil
    case 1 => makeMove(List(Nil))
    case n => makeMove(makeNMove(n - 1))
  }

  println(makeNMove(1))
  println(makeNMove(2))

  def filterA[F[_]: Alternative, A](fa: F[A])(cond: A => Boolean): F[A] = {
    var acc = Alternative[F].empty[A]
    Alternative[F].map(fa){ x =>
      if (cond(x)) acc = Alternative[F].combineK(acc, Alternative[F].pure(x))
      else ()
    }
    acc
  }

  def positionOf(p: List[Move], c: Character): Position = {
    def positionFromCount(n: Int): Position = {
      if (n % 2 == 0) West
      else East
    }
    c match {
      case Farmer => positionFromCount(p.size)
      case x => positionFromCount(filterA(p)(_ == Move(c)).size)
    }
  }

  val p = List(Move(Goat), Move(Farmer), Move(Wolf), Move(Goat))
  println(positionOf(p, Farmer))
  println(positionOf(p, Wolf))

  def isSolution(p: List[Move]) = {
    val pos = (List(p) |@| possibleMoves map { (p, m) => positionOf(p, m.x)})
    filterA(pos)(_ == West).isEmpty
  }

  def moveLegal(p: List[Move], m: Move): Boolean = positionOf(p, Farmer) == positionOf(p, m.x)

  println(moveLegal(p, Move(Wolf)))

  def safePlan(p: List[Move]): Boolean = {
    val posGoat = positionOf(p, Goat)
    val posFarmer = positionOf(p, Farmer)
    val safeGoat = posGoat != positionOf(p, Wolf)
    val safeCabbage = positionOf(p, Cabbage) != posGoat
    (posFarmer == posGoat) || (safeGoat && safeCabbage)
  }

  def makeMoves(ps: List[List[Move]]): List[List[Move]] = (ps |@| possibleMoves) map { (p, m) =>
    if (!moveLegal(p, m)) Nil
    else if (!safePlan(List(m) <+> p)) Nil
    else List(m) <+> p
  }

  def makeNMoves(n: Int): List[List[Move]] = n match {
    case 0 => Nil
    case 1 => makeMoves(List(Nil))
    case n => makeMoves(makeNMoves(n -1))
  }

  def findSolution(n: Int): Unit = filterA(makeNMoves(n))(isSolution) map { p =>
    println(p map (_.show))
  }

  findSolution(6)
  findSolution(7)
  findSolution(8)
}
