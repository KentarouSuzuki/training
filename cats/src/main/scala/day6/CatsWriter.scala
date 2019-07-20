package day6

import cats._
import cats.data._
import cats.implicits._
import cats.laws.discipline.MonadTests

object CatsWriter extends App {
  def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")

  implicit class PairOpsSemigroup[A, B: Semigroup](pair: (A, B)){
    def applyLog[C](f: A => (C, B)): (C, B) = {
      val (x, log) = pair
      val (y, newlog) = f(x)
      (y, log |+| newlog)
    }
  }

  println((3, "Smallish gang").applyLog(isBigGang))

  val w = Writer("Smallish gang.", 3)
  val v = Writer.value[String, Int](3)
  val l = Writer.tell[String]("Log something")

  println(w.run)
  println(v.run)
  println(l.run)

  def logNumber(x: Int): Writer[List[String], Int] = Writer(List("Got number: " + x.show), 3)
  def multWithLog: Writer[List[String], Int] =
    for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b

  println(multWithLog.run)

  def gcd(a: Int, b: Int): Writer[List[String], Int] = if (b == 0) for {
    _ <- Writer.tell(List("Finished with " + a.show))
  } yield a
  else
    Writer.tell(List(s"${a.show} mod ${b.show} = ${(a % b).show}")) flatMap {_ => gcd(b, a % b)}

  println(gcd(45, 48).run._1.mkString("\n"))

  def vecrotFinalCountDown(x: Int): Writer[Vector[String], Unit] = {
    import annotation.tailrec
    @tailrec def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] = x match {
      case 0 => w flatMap {_ => Writer.tell(Vector("0"))}
      case `x` => doFinalCountDown(x - 1, w flatMap { _ => Writer.tell(Vector(x.show))})
    }
    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, Writer.tell(Vector[String]()))
    val t1 = System.currentTimeMillis

    r flatMap {_ => Writer.tell(Vector((t1 - t0).show + "msec"))}
  }

  def listFinalCountDown(x: Int): Writer[List[String], Unit] = {
    import annotation.tailrec
    @tailrec def doFinalCountDown(x: Int, w: Writer[List[String], Unit]): Writer[List[String], Unit] = x match {
      case 0 => w flatMap {_ => Writer.tell(List("0"))}
      case `x` => doFinalCountDown(x - 1, w flatMap { _ => Writer.tell(List(x.show))})
    }

    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, Writer.tell(List[String]()))
    val t1 = System.currentTimeMillis

    r flatMap {_ => Writer.tell(List((t1 - t0).show + "msec"))}
  }

  println(vecrotFinalCountDown(10000).run._1.lastOption)
  println(listFinalCountDown(10000).run._1.lastOption)
}
