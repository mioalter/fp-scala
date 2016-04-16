package interpreters

object Tree {

  import language.higherKinds
  import UsefulTraits._
  import Implicits._

  sealed trait Tree[A]
  final case class Leaf[A](value : A) extends Tree[A]
  final case class Node[A](left : Tree[A], right : Tree[A]) extends Tree[A]


  implicit object TreeMonad extends Monad[Tree] {
    // This way of making Tree a monad comes from 
    // my Haskell meetup talk
    def map[A,B](ta : Tree[A])(f : A => B) : Tree[B] =
      ta match {
        case Leaf(x) => Leaf(f(x))
        case Node(l,r) => Node(map(l)(f), map(r)(f))
      }
    def pure[A](a : A) : Tree[A] = Leaf(a)
    def bind[A,B](ta : =>Tree[A])(f : A => Tree[B]) : Tree[B] =
      ta match {
        case Leaf(x) => f(x)
        case Node(l,r) => Node(bind(l)(f), bind(r)(f))
      }
  }

}
