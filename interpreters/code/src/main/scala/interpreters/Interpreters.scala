package interpreters

object Interpreters {

  import language.higherKinds
  import UsefulTraits._
  import Identity._
  import Exp._
  import Tree._
  
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