package day13

import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._

case class User(id: Long, name: String)

sealed trait Error

object Error {
  final case class UserNotFound(userId: Long) extends Error
  final case class ConnectionError(message: String) extends Error
}

trait UserRepos[F[_]] {
  implicit def F: Monad[F]
  def userRepo: UserRepo
  trait UserRepo {
    def followers(userId: Long): F[List[User]]
  }
}

class UserRepos0(implicit ec: ExecutionContext) extends UserRepos[Future] {
  override val F = implicitly[Monad[Future]]
  override val userRepo: UserRepo = new UserRepo0{}
  trait UserRepo0 extends UserRepo {
    override def followers(userId: Long): Future[List[User]] = Future.successful{Nil}
  }
}

class TestUserRepos extends UserRepos[Id] {
  override val F = implicitly[Monad[Id]]
  override val userRepo: UserRepo = new UserRepo0 {}
  trait UserRepo0 extends UserRepo {
    override def followers(userId: Long): Id[List[User]] = userId match {
      case 0L => List(User(1, "Michel"))
      case 1L => List(User(0, "Vito"))
      case x => sys.error("not found")
    }
  }
}

trait UserServices[F[_]] { this: UserRepos[F] =>
  def userService: UserService = new UserService
  class UserService {
    def isFriends(user1: Long, user2: Long): F[Boolean] =
      F.flatMap(userRepo.followers(user1)){ a =>
        F.map(userRepo.followers(user2)){ b =>
          a.exists(_.id == user2) && b.exists(_.id == user1)
        }
      }
  }
}

class UserRepos1(implicit ec: ExecutionContext) extends UserRepos[EitherT[Future, Error, ?]] {
  override val F = implicitly[Monad[EitherT[Future, Error, ?]]]
  override val userRepo: UserRepo = new UserRepo1 {}
  trait UserRepo1 extends UserRepo {
    override def followers(userId: Long): EitherT[Future, Error, List[User]] = userId match {
      case 0L => EitherT.right(Future {List(User(1, "Michael"))})
      case 1L => EitherT.right(Future {List(User(0, "Vito"))})
      case x =>
        EitherT.left(Future.successful{Error.UserNotFound(x)})
    }
  }
}

object AbstractFuture extends App{
  val service = new UserRepos0()(ExecutionContext.global)
  val xs = service.userRepo.followers(1L)
  println(xs)

  val testRepo = new TestUserRepos {}
  val ys = testRepo.userRepo.followers(1L)
  println(ys)

  val testService = new TestUserRepos with UserServices[Id] {}
  val isFriends = testService.userService.isFriends(0L, 1L)
  println(isFriends)

  val service1 = {
    import ExecutionContext.Implicits._
    new UserRepos1 with UserServices[EitherT[Future, Error, ?]]
  }
  val res = Await.result(service1.userService.isFriends(0L, 1L).value, 1 second)
  println(res)
}
