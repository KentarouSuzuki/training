package day10

import cats._
import cats.data._
import cats.implicits._
import java.net.URI

case class User(id: Long, parentId: Long, name: String, email: String)
trait UserRepo {
  def get(id: Long): Option[User]
  def find(name: String): Option[User]
}

trait HttpService {
  def get(uri: URI): String
}
trait Config {
  def userRepo: UserRepo
  def httpService: Option[HttpService]
}

object MonadTransformer extends App{
  type ReaderTOption[A, B] = Kleisli[Option, A, B]
  object ReaderTOption {
    def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
  }

  type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]
  object StateTReaderTOption {
    def state[C, S, A](f: S => (S, A)): StateTReaderTOption[C, S, A] =
      StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]{
        s: S => Monad[({type l[X] = ReaderTOption[C, X]})#l].pure(f(s))
      }

    def get[C, S]: StateTReaderTOption[C, S, S] =
      state {s => (s, s)}

    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = state {_ => (s, ())}

    def ro[C, S, A](f: C => Option[A]): StateTReaderTOption[C, S, A] =
      StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]{
        s: S =>
          ReaderTOption.ro[C, (S, A)]{
            c: C => f(c) map {(s, _)}
          }
      }
  }

  trait Users {
    def getUser[S](id: Long): StateTReaderTOption[Config, S, User] =
      StateTReaderTOption.ro[Config, S, User] {
        config => config.userRepo.get(id)
      }
    def findUser[S](name: String): StateTReaderTOption[Config, S, User] =
      StateTReaderTOption.ro[Config, S, User] {
        config => config.userRepo.find(name)
      }
  }

  trait Https {
    def getHttp(uri: URI): ReaderTOption[Config, String] =
      ReaderTOption.ro {
        config => config.httpService map {_.get(uri)}
      }
  }

  trait Program extends Users with Https {
    def userSearch(id: Long): StateTReaderTOption[Config, Stack, Unit] =
      for {
        u <- getUser(id)
        a <- push(u.name)
      } yield a
  }

  object ProgramRunner extends Program {
    def run(s: Stack, config: Config): Option[(Stack, Unit)] = userSearch(2).run(s).run(dummyConfig)
  }
  val dummyConfig: Config = new Config {
    val testUsers = List(
      User(0, 0, "Vito", "vito@example.com"),
      User(1, 0, "Michael", "michael@example.com"),
      User(2, 0, "Fredo", "fredo@example.com")
    )
    def userRepo: UserRepo = new UserRepo {
      override def get(id: Long): Option[User] = testUsers find {_.id === id}
      override def find(name: String): Option[User] = testUsers find {_.name === name}
    }
    def httpService: Option[HttpService] = None
  }

  type Stack =List[String]
  val pop: StateTReaderTOption[Config, Stack, String] =
    for {
      s <- StateTReaderTOption.get[Config, Stack]
      (x :: xs) = s
      _ <- StateTReaderTOption.put(xs)
    } yield x

  def push(x: String): StateTReaderTOption[Config, Stack, Unit] =
    for {
      xs <- StateTReaderTOption.get[Config, Stack]
      r <- StateTReaderTOption.put(x :: xs)
    } yield r

  def stackManip: StateTReaderTOption[Config, Stack, String] =
    for {
      _ <- push("Fredo")
      a <- pop
      b <- pop
    } yield b

  val result = stackManip.run(List("Hyman Roth")).run(dummyConfig)
  println(result)

  val result2 = ProgramRunner.run(List("Hyman Roth"), dummyConfig)
  println(result)
}
