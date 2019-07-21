package day8

import org.scalacheck.Prop.forAll

object CatsFeeMonoid extends App{
  def i(x: Char): Set[String] = Set(x.toString)
  def f(x: Char): Set[Int] = Set(x.toInt)

  println(f('a'))

  val fHom: PartialFunction[String, Int] =
    { case mx: String if mx.length == 1 => mx.charAt(0).toInt}

  def fHomSet(smx: Set[String]): Set[Int] = smx map fHom

  val g = fHomSet _ compose i

  println(g('a'))

  val propMAFree = forAll {c: Char => f(c) == g(c)}
  propMAFree.check
}
