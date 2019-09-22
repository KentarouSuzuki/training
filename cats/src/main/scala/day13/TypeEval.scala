package day13

import cats._
import cats.data._
import cats.implicits._

import scala.util.{Failure, Try}

object DangerOddEven {
  def odd(n: Int): String = even(n - 1)
  def even(n: Int): String = if(n <= 0) "done" else odd(n - 1)
}

object SafeOddEven {
  def odd(n: Int): Eval[String] = Eval.defer{even(n - 1)}
  def even(n: Int): Eval[String] = Eval.now { n <= 0 } flatMap {
    case true => Eval.now{"done"}
    case _ => Eval.defer{odd(n - 1)}
  }
}

object TypeEval extends App{
  var g: Int = 0

  val x = Eval.later {
    g = g + 1
    g
  }
  g = 2
  println("once eval later", g, x.value)
  println("twice eval later", g, x.value)

  val y = Eval.now {
    g = g + 1
    g
  }
  println("once eval now", g, y.value)
  println("twice eval now", g, y.value)

  val z = Eval.always {
    g = g + 1
    g
  }
  println("once eval always", g, z.value)
  println("twice eval always", g, z.value)

  val dangerEval: Try[String] = try {
    Try(DangerOddEven.even(200000))
  } catch {
    case e: Throwable => Failure(e)
  }
  val deferEval:Try[String]  = try{
    Try(SafeOddEven.even(200000).value)
  } catch {
    case e: Throwable => Failure(e)
  }

  println("danger even is ", dangerEval.getOrElse(s"failure"))
  println("safe even is ", deferEval.getOrElse(s"failure"))
}
