package day11

import cats._
import cats.data._
import cats.implicits._

sealed abstract class Fix[S[_], A] extends Serializable {
  def out: S[Fix[S, A]]
}
object Fix {
  case class In[S[_], A](out: S[Fix[S, A]]) extends Fix[S, A]
}

sealed trait ListF[+Next, +A]
object ListF {
  case class NilF() extends ListF[Nothing, Nothing]
  case class ConsF[A, Next](a: A, n: Next) extends ListF[Next, A]
}

object DGP {
  def map[F[_, _]: Bifunctor, A1, A2](fa: Fix[F[?, A1], A1])(f: A1 => A2): Fix[F[?, A2], A2] = Fix.In[F[?, A2], A2](Bifunctor[F].bimap(fa.out)(map(_)(f), f))

  def fold[F[_, _]: Bifunctor, A1, A2](fa: Fix[F[?, A1], A1])(f: F[A2, A1] => A2): A2 = {
    val g = (fa1: F[Fix[F[?, A1], A1], A1]) => Bifunctor[F].leftMap(fa1) {fold(_)(f)}
    f(g(fa.out))
  }

  def unfold[F[_, _]: Bifunctor, A1, A2](x: A2)(f: A2 => F[A2, A1]): Fix[F[?, A1], A1] = Fix.In[F[?, A1], A1](Bifunctor[F].leftMap(f(x))(unfold[F, A1, A2](_)(f)))
}

sealed trait TreeF[+Next, +A]
object TreeF {
  case class EmptyF() extends TreeF[Nothing, Nothing]
  case class NodeF[Next, A](a: A, left: Next, right: Next) extends TreeF[Next, A]
}

object GenericPrograming extends App {
  type GenericList[A] = Fix[ListF[+?, A], A]
  object GenericList {
    def nil[A]: GenericList[A] = Fix.In[ListF[+?, A], A](ListF.NilF())
    def cons[A](a: A, xs: GenericList[A]): GenericList[A] = Fix.In[ListF[+?, A], A](ListF.ConsF(a, xs))
  }
  import GenericList._

  println(cons(1, nil))

  implicit val listFBifunctor: Bifunctor[ListF] = new Bifunctor[ListF] {
    override def bimap[S1, A1, S2, A2](fab: ListF[S1, A1])(f: S1 => S2, g: A1 => A2): ListF[S2, A2] = fab match {
      case ListF.NilF() => ListF.NilF()
      case ListF.ConsF(a, next) => ListF.ConsF(g(a), f(next))
    }
  }

  println(
    DGP.map(cons(1, nil)){_ + 1}
  )

  def pred(n: Int): GenericList[Int] = DGP.unfold[ListF, Int, Int](n) {
    case 0 => ListF.NilF()
    case n => ListF.ConsF(n, n - 1)
  }
  println(pred(5))

  type Tree[A] = Fix[TreeF[?, A], A]
  object Tree {
    def empty[A]: Tree[A] = Fix.In[TreeF[+?, A], A](TreeF.EmptyF())
    def node[A, Next](a: A, left: Tree[A], right: Tree[A]): Tree[A] = Fix.In[TreeF[+?, A], A](TreeF.NodeF(a, left, right))
  }
  import Tree._

  println(
    node(2, node(1, empty, empty), empty)
  )

  implicit val treeFBifunctor: Bifunctor[TreeF] = new Bifunctor[TreeF] {
    override def bimap[A, B, C, D](fab: TreeF[A, B])(f: A => C, g: B => D): TreeF[C, D] = fab match {
      case TreeF.EmptyF() => TreeF.EmptyF()
      case TreeF.NodeF(a, left, right) => TreeF.NodeF(g(a), f(left), f(right))
    }
  }

  def sum(tree: Tree[Int]): Int = DGP.fold[TreeF, Int, Int](tree) {
    case TreeF.EmptyF() => 0
    case TreeF.NodeF(a, l, r) => a + l + r
  }
  println(
    sum(node(2, node(1, empty, empty), empty))
  )

  def grow[A: PartialOrder](xs: List[A]): Tree[A] = DGP.unfold[TreeF, A, List[A]](xs){
    case Nil => TreeF.EmptyF()
    case x :: xs => {
      import cats.syntax.partialOrder._
      TreeF.NodeF(x, xs filter{_ <= x}, xs filter {_ > x})
    }
  }
  println(
    grow(List(3, 1, 4, 2))
  )
}
