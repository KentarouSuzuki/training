package day8

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

case class LongProduct(value: Long)

object CatsFlatMap extends App{
  implicit val longProdMonoid: Monoid[LongProduct] = new Monoid[LongProduct]{
    override def empty: LongProduct = LongProduct(1)
    override def combine(x: LongProduct, y: LongProduct): LongProduct = LongProduct(x.value * y.value)
  }

  def powWriter(x: Long, exp: Long): Writer[LongProduct, Unit] =
    exp match {
      case 0 => Writer(LongProduct(1L), ())
      case _ => Writer(LongProduct(x), ()) >>= {_ => powWriter(x, exp - 1)}
    }

  println(powWriter(2, 3).run)

  def tailRecM[A, B] = FlatMap[Writer[Vector[String], ?]].tailRecM[A, B] _
  def powWriter2(x: Long, exp: Long): Writer[LongProduct, Unit] =
    FlatMap[Writer[LongProduct, ?]].tailRecM(exp) {
      case 0L => Writer.value[LongProduct, Either[Long, Unit]](Right(()))
      case m: Long => Writer.tell(LongProduct(x)) >>= { _ => Writer.value(Left(m - 1))}
    }

  println(powWriter(2, 3).run)
  println(powWriter2(1, 10000).run)
}
