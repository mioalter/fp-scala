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

  val EFree2Id : (EFree ~> Id) = new (EFree ~> Id) {
    def apply[A](efree : EFree[A]) : Id[A] = 
      efree match {
        case Const(x) => Free2Id(x)
        case Add(x,y) => add[A](Free2Id(x), Free2Id(y))
      }
  }

  val Free2Id : (FExp ~> Id) = new (FExp ~> Id) {
    def apply[A](fexp : FExp[A]) : Id[A] =
      fexp match {
        case Return(x) => x
        case Suspend(s) => EFree2Id(s)

      }
  }


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
  // val Tree2Id = new (Tree ~> Id) {    
  //   def apply[A](t : Tree[A]) : Id[A] = treeSum(t)
  // }

  // val Exp2Id = new (Exp ~> Id) {
  //   def apply[A](exp : Exp[A]) : Id[A] =
  // }
}
