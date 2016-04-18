package interpreters

object Identity {

  import language.higherKinds
  import UsefulTraits._
  import Implicits._

  type Id[A] = A 
  // avoid lots of wrapping and unwrapping
  // this is a type synonym so Id[A] and A are interchangeable

  implicit object IdentityMonad extends Monad[Id] {
    def map[A,B](ida : Id[A])(f : A => B) : Id[B] = f(ida)
    def pure[A](a : A) : Id[A] = a
    def bind[A,B](ida : =>Id[A])(f : A => Id[B]) : Id[B] = f(ida)
  }

  def add[A](x : A, y : A)(implicit ev : Monoid[A] = null) : A = {
    if (ev != null) {ev.plus(x,y)}
    else x
  }

  // def treeSum[A](t : Tree[A])(implicit ev : Monoid[A] = null) : A =
  //   t match {
  //     case Leaf(x) => x
  //     case Node(l,r) if (ev != null) => treeSum(l) + treeSum(r)
  //     case Node(l,r) => treeSum(l)
  //   }  
}
