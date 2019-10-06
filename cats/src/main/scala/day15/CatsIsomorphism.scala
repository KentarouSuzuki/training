package day15

import cats._
import cats.data._
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen, Prop}

object Isomorphisms {
  trait Isomorphism[Arrow[_, _], A, B] {self =>
    def to: Arrow[A, B]
    def from : Arrow[B, A]
  }
  type IsoSet[A, B] = Isomorphism[Function1, A, B]
  type <=> [A, B] = IsoSet[A, B]
}

sealed trait Family
case object Mother extends Family
case object Father extends Family
case object Child extends Family

sealed trait Relic
case object Feather extends Relic
case object Stone extends Relic
case object Flower extends Relic

object CatsIsomorphism extends App{
  import Isomorphisms._
  val isoFamilyRelic = new (Family <=> Relic) {
    val to: Family => Relic = {
      case Mother => Feather
      case Father => Stone
      case Child => Flower
    }
    val from: Relic => Family = {
      case Feather => Mother
      case Stone => Father
      case Flower => Child
    }
  }

  def func1EqualsProp[A, B](f: A => B, g: A => B)(implicit ev1: Eq[B], ev2: Arbitrary[A]): Prop = Prop.forAll {a: A =>
    f(a) === g(a)
  }

  val p1 = func1EqualsProp((_: Int) + 2, 1 + (_: Int))
  println(p1.check)

  val p2 = func1EqualsProp((_: Int) + 2, 2 + (_: Int))
  println(p2.check)

  implicit val familyEqual = Eq.fromUniversalEquals[Family]
  implicit val relicEqual = Eq.fromUniversalEquals[Relic]
  implicit val arbFamily: Arbitrary[Family] = Arbitrary {
    Gen.oneOf(Mother, Father, Child)
  }
  implicit val arbRelic: Arbitrary[Relic] = Arbitrary {
    Gen.oneOf(Feather, Stone, Flower)
  }

  func1EqualsProp(isoFamilyRelic.from compose isoFamilyRelic.to, identity[Family] _).check
  func1EqualsProp(isoFamilyRelic.to compose isoFamilyRelic.from, identity[Relic] _).check
}
