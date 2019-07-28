package day10

import cats._
import cats.data._
import cats.implicits._
import scala.concurrent.{Future, ExecutionContext, Await, duration}
import duration._
import ExecutionContext.global


case class Users(id: Long, name: String)

sealed trait Error
object Error {
  final case class UserNotFound(userId: Long) extends Error
  final case class ConnectionError(message: String) extends Error
}
object UserRepository {
  def followers(userId: Long)(implicit ex: ExecutionContext): EitherT[Future, Error, List[Users]] =
    userId match {
      case 0L => EitherT.right(Future {List(Users(1, "Michael"))})
      case 1L => EitherT.right(Future {List(Users(0, "Vito"))})
      case x =>
        println("not found")
        EitherT.left(Future.successful{ Error.UserNotFound(x)})
    }
}

object FutureEither extends App{
  def isFriends(user1: Long, user2: Long)(implicit ec: ExecutionContext): EitherT[Future, Error, Boolean] =
    for {
      a <- UserRepository.followers(user1)
      b <- UserRepository.followers(user2)
    } yield a.exists(_.id == user2) && b.exists(_.id == user1)

  val result1 = Await.result(isFriends(0, 1)(global).value, 1 second)
  val result2 = Await.result(isFriends(2, 3)(global).value ,1 second)
  println(result1)
  println(result2)
}
