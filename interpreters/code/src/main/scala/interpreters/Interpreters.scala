package interpreters

object Interpreters {

  import language.higherKinds
  import UsefulTraits._
  import Identity._
  import Free._
  import Exp._
  import Tree._
  import Implicits._

  val Id2Id  = new (Id ~> Id) {
    def apply[A](ida : Id[A]) : Id[A] = ida
  }
  
  type FExp[A] = Free[Exp,A]

  def FExpInterpreter[A](fexp : FExp[A])(implicit ev : Monoid[A]) : A =
    fexp match {
      case Return(x) => x
      case Suspend(s) => 
        s match {
          case Const(x) => FExpInterpreter(x)
          case Add(x,y) => ev.plus(FExpInterpreter(x),FExpInterpreter(y))
        }
    }

  val Exp2Tree = new (Exp ~> Tree) {
    def apply[A](exp : Exp[A]) = 
      exp match {
        case Const(x) => Leaf(x)
        case Add(x,y) => Node(Leaf(x), Leaf(y))
      }
  }

  def TreeInterpreter[A](t : Tree[A])(implicit ev : Monoid[A]) : A = 
    t match {
      case Leaf(x) => x
      case Node(l,r) => ev.plus(TreeInterpreter(l), TreeInterpreter(r))
    }
  
}
