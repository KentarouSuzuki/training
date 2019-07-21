package day8

import cats._
import cats.free.Free
import cats.data._
import cats.implicits._

sealed trait Toy[+A, +Next]
object Toy {
  case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
  case class Bell[Next](next: Next) extends Toy[Nothing, Next]
  case class Done() extends Toy[Nothing, Nothing]
}

sealed trait CharToy[+Next]
object CharToy {
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
      case o: CharOutput[A] => CharOutput(o.a, f(o.next))
      case b: CharBell[A] => CharBell(f(b.next))
      case CharDone() => CharDone()
    }
  }

  def output[Next](a: Char): Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharOutput(a, ()))
  def bell: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharBell(()))
  def done: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharDone())
  def pure[A](a: A): Free[CharToy, A] = Free.pure[CharToy, A](a)
}

object CatsFree extends App{
  import CharToy._

  val subroutine = output('A')
  val program = for {
    _ <- subroutine
    _ <- bell
    _ <- done
  } yield ()

  println(s"output(A): $subroutine")
  println(s"program: $program")

  def showProgram[R: Show](p: Free[CharToy, R]): String = p.fold(
    {r: R => s"return ${Show[R].show(r)}\n"},
    {
      case CharOutput(a, next) => s"output ${Show[Char].show(a)}\n${showProgram(next)}"
      case CharBell(next) => s"bell \n${showProgram(next)}"
      case CharDone() => "done\n"
    }
  )

  println(showProgram(program))
  println(showProgram(pure('A') flatMap output))
  println(showProgram((output('A') flatMap {_ => done}) flatMap {_ => output('C')}))
  println(showProgram(output('A') flatMap {_ => done flatMap{_ => output('C')} }))
}
