package day7

import cats._
import cats.data._
import cats.implicits._

object CatsIor extends App{
  println(Ior.right[NonEmptyList[String], Int](1))
  println(Ior.left[NonEmptyList[String], Int](NonEmptyList.of("error")))
  println(Ior.both[NonEmptyList[String], Int](NonEmptyList.of("warning"), 1))

  println(
    Ior.right[NonEmptyList[String], Int](1) >>= {x => Ior.right[NonEmptyList[String], Int](x + 1)}
  )
  println(
    Ior.left[NonEmptyList[String], Int](NonEmptyList.of("error 1")) >>= { x => Ior.right[NonEmptyList[String], Int](x + 1)}
  )
  println(
    Ior.both[NonEmptyList[String], Int](NonEmptyList.of("warning 1"), 1) >>= {x => Ior.right(x + 1)}
  )

  val res = for {
    e1 <- Ior.right[NonEmptyList[String], Int](1)
    e2 <- Ior.both[NonEmptyList[String], Int](NonEmptyList.of("event 2 warning"), e1 + 1)
    e3 <- Ior.both[NonEmptyList[String], Int](NonEmptyList.of("event 3 warning"), e2 + 1)
  } yield e1 |+| e2 |+| e3
  println(res)
}
