package interpreters

object Interpreters {

  import language.higherKinds
  import UsefulTraits._
  import Identity._
  import Exp._
  import Tree._
  import Implicits._

  val Id2Id  = new (Id ~> Id) {
    def apply[A](ida : =>Id[A]) : Id[A] = ida
  }
  
  val Exp2Id = new (Exp ~> Id) {
    def apply[A](exp : =>Exp[A]) : Id[A] =
      exp match {
        case Const(x) => x
        case Add(x,y) => add(x,y)
      }
  }

  val Exp2Tree = new (Exp ~> Tree) {
    def apply[A](exp : =>Exp[A]) = 
      exp match {
        case Const(x) => Leaf(x)
        case Add(x,y) => Node(Leaf(x), Leaf(y))
      }
  }


}
