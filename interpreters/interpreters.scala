
object UsefulTraits{

  import language.higherKinds

  trait Monoid[A] {
    def zero : A
    def plus(a : A, b : A) : A
  }

  trait Functor[F[_]] {
    def map[A,B](fa : F[A])(f : A => B) : F[B]
  }  

  trait Monad[M[_]] extends Functor[M] {
    def pure[A](a : A) : M[A]
    def bind[A,B](ma : M[A])(f : A => M[B]) : M[B]
  }

  trait ~>[F[_],G[_]] {
    def apply[A](fa : F[A]) : G[A]
  }

}



object TypesAndInstances {

  import language.higherKinds
  import UsefulTraits._
  
  type Id[A] = A 
  // avoid lots of wrapping and unwrapping
  // this is a type synonym so Id[A] and A are interchangeable

  sealed trait Exp[T]
  final case class Const[T](s : T) extends Exp[T]
  final case class Add[T](a : T, b : T) extends Exp[T]

  sealed trait Tree[A]
  final case class Leaf[A](value : A) extends Tree[A]
  final case class Node[A](left : Tree[A], right : Tree[A]) extends Tree[A]

  sealed trait Free[F[_],A]{
    def map[B](f : A => B)(implicit ev : Functor[F]) : Free[F,B] = 
      this match {
        case Return(a) => Return(f(a))
        case Suspend(s) => Suspend(ev.map(s)(_.map(f))) 
        // s = F[y] for some y : Free[F,A]
        // By our recursive construction, Free[F,A].map(f) knows how to eat y
        // then we F.map it over s = F[y]

      }
    //def pure[A](a : A) : Free[F,A] = Return(a)
    def bind[B](f : A => Free[F,B])(implicit ev : Functor[F]) : Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Suspend(s) => Suspend(ev.map(s)(_.bind(f)))
      }
    def foldMap[G[_]](f : F ~> G)(implicit ev : Monad[G]) : G[A] = 
      this match {
        case Return(a) => ev.pure(a)
        case Suspend(s) => ev.bind(f(s))(_.foldMap(f)) // f(s) >>= foldMap(f) in Haskell 
      } 
  }
  case class Return[F[_],A](a : A) extends Free[F,A]
  case class Suspend[F[_],A](s : F[Free[F,A]]) extends Free[F,A]


  implicit object TreeMonad extends Monad[Tree] {
    // This way of making Tree a monad comes from 
    // my Haskell meetup talk
    def map[A,B](ta : Tree[A])(f : A => B) : Tree[B] =
      ta match {
        case Leaf(x) => Leaf(f(x))
        case Node(l,r) => Node(map(l)(f), map(r)(f))
      }
    def pure[A](a : A) : Tree[A] = Leaf(a)
    def bind[A,B](ta : Tree[A])(f : A => Tree[B]) : Tree[B] =
      ta match {
        case Leaf(x) => f(x)
        case Node(l,r) => Node(bind(l)(f), bind(r)(f))
      }
  }

  implicit object IdentityMonadInstance extends Monad[Id] {
    def map[A,B](ida : Id[A])(f : A => B) : Id[B] = f(ida)
    def pure[A](a : A) : Id[A] = a
    def bind[A,B](ida : Id[A])(f : A => Id[B]) : Id[B] = f(ida)
  }

  implicit object ExpFunctorInstance extends Functor[Exp] {
    def map[T,U](e : Exp[T])(f : T => U) : Exp[U] = 
      e match {
        case Const(s) => Const(f(s))
        case Add(a,b) => Add(f(a),f(b))
      }    
  }

  implicit object IntMonoid extends Monoid[Int] {
    def zero : Int = 0
    def plus(a : Int, b : Int) = a + b
  }


  def map[F[_],A,B](fa : F[A])(f : A => B)(implicit ev : Functor[F]) : F[B] = ev.map(fa)(f)
  def pure[M[_],A](a : A)(implicit ev : Monad[M]) : M[A] = ev.pure(a)
  def bine[M[_],A,B](ma : M[A])(f : A => M[B])(implicit ev : Monad[M]) : M[B] = ev.bind(ma)(f)

}

object Interpreters {

  import language.higherKinds
  import UsefulTraits._
  import TypesAndInstances._

  def Id2Id() : (Id ~> Id) = new (Id ~> Id) {
    def apply[A](ida : Id[A]) : Id[A] = ida    
  }

  // def Exp2Id() : (Exp ~> Id) = new (Exp ~> Id) {
  //   def apply[A](exp : Exp[A])(implicit ev: Monoid[A]) : Id[A] =
  //     exp match {
  //       case Const(x) => x
  //       case Add(x,y) => ev.plus(x,y)
  //     }
  // }

  def Exp2Tree() : (Exp ~> Tree) = new (Exp ~> Tree) {
    def apply[A](exp : Exp[A]) = 
      exp match {
        case Const(x) => Leaf(x)
        case Add(x,y) => Node(Leaf(x), Leaf(y))
      }
  }


}


import TypesAndInstances._
import Interpreters._


type IntExp = Free[Exp,Int]

def f(x : Int) : Boolean = {x % 2 == 0}
def g(x : Int) : IntExp = Suspend(Add(Return(x), Return(x+1)))
val returnExp : IntExp = Return(1)
val constExp : IntExp = Suspend(Const(Return(2)))
val addExp : IntExp = Suspend(Add(Return(3),Return(4)))


val moreAdd = addExp.bind(g)
// Suspend(Add(Suspend(Add(Return(3),Return(4))),Suspend(Add(Return(4),Return(5)))))
val moreAddInterpreted = moreAdd.foldMap[Tree](Exp2Tree)
// Node(Node(Leaf(3),Leaf(4)),Node(Leaf(4),Leaf(5)))
// So Tree is (isomorphic to) the free monad on Exp



