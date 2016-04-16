package interpreters

object Run{
	
  import UsefulTraits._	
  import Identity._
  import Exp._
  import Tree._
  import Free._
  import Implicits._
  import Interpreters._

  type IntExp = Free[Exp,Int]
  
  def f(x : Int) : Boolean = {x % 2 == 0}
  def g(x : Int) : IntExp = Suspend(Add(Return(x-1), Return(x+1)))
  val returnExp : IntExp = Return(1)
  val constExp : IntExp = Suspend(Const(Return(2)))
  val addExp : IntExp = Suspend(Add(Return(3),Return(4)))
  
  val addExp2Id = addExp.foldMap[Id](Exp2Id) 

  val moreAdd = addExp.bind(g)
  // Suspend(Add(Suspend(Add(Return(3),Return(4))),Suspend(Add(Return(4),Return(5)))))
  val moreAdd2Tree = moreAdd.foldMap[Tree](Exp2Tree)
  // Node(Node(Leaf(3),Leaf(4)),Node(Leaf(4),Leaf(5)))
  // So Tree is (isomorphic to) the free monad on Exp  
  val moreAdd2Id = moreAdd.foldMap[Id](Exp2Id)
}
