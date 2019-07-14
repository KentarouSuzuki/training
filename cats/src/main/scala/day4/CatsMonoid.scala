package day4

import cats._
import cats.data._
import cats.implicits._

import algebra.laws.GroupLaws
import org.scalacheck.{Arbitrary, Gen}

class Disjunction(val unwrap: Boolean) extends AnyVal
object Disjunction {
  @inline def apply(b: Boolean): Disjunction = new Disjunction(b)
  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    def combine(a1: Disjunction, a2: Disjunction): Disjunction =
      Disjunction(a1.unwrap || a2.unwrap)
    def empty: Disjunction = Disjunction(false)
  }

  implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
    override def eqv(a1: Disjunction, a2: Disjunction): Boolean = a1.unwrap == a2.unwrap
  }
}

class Conjunction(val unwrap: Boolean) extends AnyVal
object Conjunction {
  @inline def apply(b: Boolean): Conjunction = new Conjunction(b)
  implicit val conjuntionMonoid: Monoid[Conjunction] = new Monoid[Conjunction] {
    override def combine(a1: Conjunction, a2: Conjunction): Conjunction =
      Conjunction(a1.unwrap && a2.unwrap)
    override def empty: Conjunction = Conjunction(true)
  }

  implicit val conjunctionEq: Eq[Conjunction] = new Eq[Conjunction] {
    def eqv(a1: Conjunction, a2: Conjunction): Boolean = a1.unwrap == a2.unwrap
  }
}

case class First[A: Eq](unwrap: Option[A])
object First {
  implicit def firstMonoid[A: Eq]: Monoid[First[A]] = new Monoid[First[A]] {
    override def combine(a1: First[A], a2: First[A]): First[A] = First((a1.unwrap, a2.unwrap) match {
      case (Some(x), _) => Some(x)
      case (None, y) => y
    })

    override def empty: First[A] = First(None: Option[A])
  }

  implicit def firstEq[A: Eq]: Eq[First[A]] = new Eq[First[A]] {
    def eqv(a1: First[A], a2: First[A]): Boolean = Eq[Option[A]].eqv(a1.unwrap, a2.unwrap)
  }
}

case class Last[A: Eq](unwrap: Option[A])
object Last {
  implicit def lastMonoid[A: Eq]: Monoid[Last[A]] = new Monoid[Last[A]] {
    override def combine(a1: Last[A], a2: Last[A]): Last[A] = Last((a1.unwrap, a2.unwrap) match {
      case (_, Some(y)) => Some(y)
      case (x, None) => x
    })

    override def empty: Last[A] = Last(None: Option[A])
  }

  implicit def lastEq[A: Eq]: Eq[Last[A]] = new Eq[Last[A]] {
    def eqv(a1: Last[A], a2: Last[A]): Boolean = Eq[Option[A]].eqv(a1.unwrap, a2.unwrap)
  }
}

object CatsMonoid extends App{
  val x1 = Disjunction(true) |+| Disjunction(false)
  val x2 = Monoid[Disjunction].empty |+| Disjunction(true)
  println(s"x1: ${x1.unwrap} \nx2: ${x2.unwrap}")

  val x3 = Conjunction(true) |+| Conjunction(false)
  val x4 = Monoid[Conjunction].empty |+| Conjunction(true)
  println(s"x3: ${x3.unwrap} \nx4: ${x4.unwrap}")

  val x5 = First('a'.some) |+| First('b'.some)
  val x6 = First(none[Char]) |+| First('c'.some)
  println(s"x5: ${x5.unwrap} \nx6: ${x6.unwrap}")

  val x7 = Last('a'.some) |+| Last('b'.some)
  val x8 = Last('c'.some) |+| Last(none[Char])
  println(s"x7: ${x7.unwrap} \nx8: ${x8.unwrap}")
}
