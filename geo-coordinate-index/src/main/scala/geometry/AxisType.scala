package geometry

import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.util.Try

trait AxisType {
  type ValidationResult[A] = ValidatedNec[IllegalArgumentException, A]

  abstract class Axis(value: AnyVal)

  case class Latitude(value: Float) extends Axis(value)
  case class Longitude(value: Float) extends Axis(value)

  def parseSexagesimal(sexagesimalValue: String) = {
    val parsePattern = """([+\-]?)(\d{1,3})°(\d{1,2})'(\d{1,2}\.?\d*)"""".r
    def stringToFloat(s: String): Either[IllegalArgumentException, Float] =
      try { Right(s.toFloat) }
      catch { case throwable: Throwable => Left(new IllegalArgumentException(s"Can't parse $s to float", throwable)) }

    sexagesimalValue match {
      case parsePattern(sign, degree, minnute, second) => for {
        deg <- stringToFloat(degree)
        min <- stringToFloat(minnute)
        sec <- stringToFloat(second)
        s = sign match { case "-" => -1; case _ => 1 }
      } yield (deg + (min / 60) + (sec / 3600)) * s
      case _ => Left(new IllegalArgumentException(s"Can't parse $sexagesimalValue to decimal"))
    }
  }.toValidatedNec

  sealed trait AxisFactory {
    val name: String
    val MINIMUM: Float
    val MAXIMUM: Float

    def validate(value: Float): ValidationResult[Float] = (validateMinimum(value), validateMaximum(value)).mapN((_, _) => value)

    private def validateMinimum(value: Float): ValidationResult[Float] = Either.cond(
      value >= MINIMUM,
      value,
      new IllegalArgumentException(s"$value is lower than $name's minimum $MINIMUM.")
    ).toValidatedNec

    private def validateMaximum(value: Float): ValidationResult[Float] = Either.cond(
      value <= MAXIMUM,
      value,
      new IllegalArgumentException(s"$value is higher than $name's maximum $MAXIMUM.")
    ).toValidatedNec
  }

  object Longitude extends AxisFactory {
    val name = "longitude"
    val MINIMUM = -180f
    val MAXIMUM = 180f

    def apply(value: Float): ValidationResult[Longitude] = validate(value).map(new Longitude(_))

    def apply(value: String): ValidationResult[Longitude] = parseSexagesimal(value) match {
      case Valid(v) => validate(v).map(new Longitude(_))
      case i@Invalid(_) => i
    }
  }

  object Latitude extends AxisFactory {
    val name = "latitude"
    val MINIMUM = -90f
    val MAXIMUM = 90f

    def apply(value: Float): ValidationResult[Latitude] = validate(value).map(new Latitude(_))

    def apply(value: String): ValidationResult[Longitude] = parseSexagesimal(value) match {
      case Valid(v) => validate(v).map(new Longitude(_))
      case i@Invalid(_) => i
    }
  }
}