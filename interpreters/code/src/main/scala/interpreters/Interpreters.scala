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
  
  type EFree[A] = Exp[Free[Exp,A]]
  type FExp[A] = Free[Exp,A]

  val FExp2Id : (FExp ~> Id) = new (FExp ~> Id) {
    def apply[A](fexp : FExp[A]) : Id[A] = 
      fexp match {
        case Return(x) => x
        case Suspend(s) => 
          s match {
            case Const(x) => FExp2Id(x)
            case Add(x,y) => add(FExp2Id(x), FExp2Id(y))
          }
      }
  } // Doesn't work

  def FExpInterpreter(fexp : FExp[Int]) : Int =
    fexp match {
      case Return(x) => x
      case Suspend(s) => 
        s match {
          case Const(x) => FExpInterpreter(x)
          case Add(x,y) => FExpInterpreter(x) + FExpInterpreter(y)
        }
    } // Works
    // So we can by-hand write a recursive function to interpret
    // There's just weird stuff going on with the add function
    // and maybe with where the monoid instance of Int is in scope

  // val Exp2Id : (Exp ~> Id) = new (Exp ~> Id) {
  //   def apply[A](exp : Exp[A]) : Id[A] = 
  //     exp match {
  //       case Const(x) if (x : FExp[A]) => x.foldMap(Exp2Id)
  //       case Const(x) => x
  //       case Add(x,y) if ((x : FExp[A]) && (y : FExp[A])) => add(x.foldMap(Exp2Id), y.foldMap(Exp2Id))
  //       case Add(x,y) => add(x,y)
  //     }
  // }

  // val EFree2Id : (EFree ~> Id) = new (EFree ~> Id) {
  //   import Implicits._
  //   def apply[A](efree : EFree[A]) : Id[A] = 
  //     efree match {
  //       case Const(x) => Free2Id(x)
  //       case Add(x,y) => add[A](Free2Id(x), Free2Id(y))
  //     }
  // }

  // val Free2Id : (FExp ~> Id) = new (FExp ~> Id) {
  //   import Implicits._
  //   def apply[A](fexp : FExp[A]) : Id[A] =
  //     fexp match {
  //       case Return(x) => x
  //       case Suspend(s) => EFree2Id(s)

  //     }
  // }


  val Exp2Tree = new (Exp ~> Tree) {
    def apply[A](exp : Exp[A]) = 
      exp match {
        case Const(x) => Leaf(x)
        case Add(x,y) => Node(Leaf(x), Leaf(y))
      }
  }

  val Tree2Id : (Tree ~> Id) = new (Tree ~> Id) {
    def apply[A](ta : Tree[A]) : Id[A] = 
      ta match {
        case Leaf(x) => x
        case Node(l,r) =>  add[A](Tree2Id(l),Tree2Id(r))
      }
  }
  // Even Tree2Id doesn't work so EFree and Free obviously won't
  // something weird going on.
}
