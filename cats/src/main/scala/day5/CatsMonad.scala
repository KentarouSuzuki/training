package day5

import cats._
import cats.data._
import cats.implicits._
import cats.laws.discipline.MonadTests

object CatsMonad extends App{
  type Birds = Int
  case class Pole(left: Birds, right: Birds){
    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[Pole]

    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[Pole]

    def banana: Option[Pole] = none[Pole]
  }

  println(Pole(0, 0).landLeft(2))
  println(Pole(1, 2).landRight(3))

  val rlr = Monad[Option].pure(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)}
  println(rlr)

  val lrlr = Monad[Option].pure(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)}
  println(lrlr)

  val lbl = Monad[Option].pure(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.banana} >>= {_.landRight(1)}
  println(lbl)

  println(3.some >> 4.some)
  println(none[Int] >> 4.some)
  println(3.some >> none[Int])

  val lnl = (Monad[Option].pure(Pole(0, 0)) >>= {_.landLeft(1)}) >> none[Pole] >>= {_.landRight(1)}
  println(lnl)

  println(3.some >>= {x => "!".some >>= {y => (x.show + y).some}})
  println(3.some >>= {x => none[String] >>= {y => (x.show + y).some}})
  println(3.some >>= {x => "!".some >>= {y => none[String]}})

  def routine: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third
  println(routine)

  def routine2: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      _ <- none[Pole]
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third
  println(routine2)

  def justH: Option[Char] = for {
    x :: xs <- "hello".toList.some
  } yield x
  println(justH)

  def wopwop: Option[Char] = for {
    x :: xs <- "".toList.some
  } yield x
  println(wopwop)

  assert{(Monad[Option].pure(3) >>= {x => (x + 100000).some}) === ({x: Int => (x + 100000).some})(3)}
  assert{("move on up".some >>= {Monad[Option].pure(_)}) === "move on up".some}

  val a1 = Monad[Option].pure(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)}
  val a2 = Monad[Option].pure(Pole(0, 0)) >>= {x =>
    x.landRight(2) >>= {y =>
      y.landLeft(2) >>= {z =>
        z.landRight(2)
      }
    }
  }
  assert {a1 == a2}
}
