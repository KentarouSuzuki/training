package day12

import cats._
import cats.data._
import cats.implicits._

import cats.data.Func.appFunc

object ApplicativeWordcount extends App {
  type Count[A] = Const[Int, A]

  def liftInt(i: Int): Count[Unit] = Const(i)
  def count[A](a: A): Count[Unit] = liftInt(1)
  val countChar: AppFunc[Count, Char, Unit] = appFunc(count)
  val text = ("Faith, I must leave thee, love, and shortly too.\n" + "My operant powers their functions leave to do.\n").toList

  println(countChar traverse text)

  def testIf(b: Boolean): Int = if (b) 1 else 0
  val countLine = appFunc{c: Char => liftInt(testIf(c === '\n'))}

  println(countLine traverse text)

  def isSpace(c: Char): Boolean = (c === ' ' || c === '\n' || c === '\t')
  val countWord = appFunc{c: Char =>
    import cats.data.State.{get, set}
    for {
      x <- get[Boolean]
      y = !isSpace(c)
      _ <- set(y)
    } yield testIf(y && !x)
  } andThen appFunc(liftInt)
  val x = countWord traverse text
  println(x.value.runA(false).value)

  val countAll = countWord product countLine product countChar
  val allResults = countAll traverse text
  val charCount = allResults.second
  val lineCount = allResults.first.second
  val wordCountState = allResults.first.first
  val wordCount = wordCountState.value.runA(false).value

  println(List(countAll, allResults, charCount, lineCount, wordCountState, wordCount).mkString("\n"))
}
