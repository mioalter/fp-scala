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
  val addExpInterpreted = FExpInterpreter(addExp)

  val moreAdd = addExp.bind(g)
  // Suspend(Add(Suspend(Add(Return(2),Return(4))),Suspend(Add(Return(3),Return(5)))))
  val moreAdd2Tree = moreAdd.foldMap[Tree](Exp2Tree)
  // Node(Node(Leaf(2),Leaf(4)),Node(Leaf(3),Leaf(5)))
  // Unsurprisingly, Tree is (isomorphic to) the free monad on Exp  

  val expIntSum = FExpInterpreter(moreAdd)
  val expStrSum = FExpInterpreter(moreAdd.map(_.toString))
  val treeSum = TreeInterpreter(moreAdd2Tree)

  // So, interpreting into a monad that has a similar structure to the free monad is easy,
  // we can do Free Exp to Tree no problem
  // It's also easy enough to write recursive interpreters by hand to interpret into Id, say,
  // but still have to figure out how to interpret into the Id monad
  // using the general machinery (foldMap)
  // Think the issue has to do with scope
}
