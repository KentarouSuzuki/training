package day7

import cats._
import cats.data._
import cats.implicits._

object CatState extends App{
  type Stack = List[Int]
  def pop(s0: Stack): (Stack, Int) = s0 match {
    case x :: xs => (xs, x)
    case Nil => sys.error("stack is empty")
  }

  def push(s0: Stack, a: Int): (Stack, Unit) = (a :: s0, ())

  def stackManip(s0: Stack): (Stack, Int) = {
    val (s1, _) = push(s0, 3)
    val (s2, a) = pop(s1)

    pop(s2)
  }

  println(stackManip(List(5, 8, 2, 1)))

  val statePop = State[Stack, Int] {
    case x :: xs => (xs, x)
    case Nil => sys.error("stack is empty")
  }

  def statePush(a: Int) = State[Stack, Unit] {
    case xs => (a :: xs, ())
  }

  def stateStackManip: State[Stack, Int] = for {
    _ <- statePush(3)
    a <- statePop
    b <- statePop
  } yield b

  println(stateStackManip.run(List(5, 7, 2, 1)).value)

  def stackyStack: State[Stack, Unit] = for {
    stackNow <- State.get[Stack]
    r <- if (stackNow === List(1, 2, 3))
      State.set[Stack](List(8, 3, 1))
      else State.set[Stack](List(9, 2, 1))
  } yield r

  println(stackyStack.run(List(1, 2, 3)).value)

  val statePop2: State[Stack, Int] = for {
    s <- State.get[Stack]
    x :: xs = s
    _ <- State.set[Stack](xs)
  } yield x

  def statePush2(x: Int): State[Stack, Unit] = for {
    xs <- State.get[Stack]
    r <- State.set(x :: xs)
  } yield r
}
