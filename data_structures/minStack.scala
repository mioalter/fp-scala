//
// How would you design a stack which, in addition to push and pop, also has a function min which returns the minimum element?
// Push, pop and min should all operate in O(1) time.


// A stack basically IS a linked list
// push and pop are just cons and head
// To access the min, we can have one stack, our stack of values
// and another stack which contains the minimum elements we've seen
// we only add an element to the min-stack if it is <= to the current min
// Then, since a stack is LIFO, the current min is always at the head of the min-stack
// To pop, we pop from the stack of minima if the head of our stack of values is a minimum

sealed trait Stack[+A]
final case object Empty extends Stack[Nothing]
final case class Stk[A](head : A, tail : Stack[A]) extends Stack[A]

// Sealed means all the case classes/objects that extend Stack have to be in this file
// so no one else can create more elsehwere/later
// BUT, Empty and Stk can be extended (subclassed) without restriction
// this is what _final_ prevents.

object Stack {
  
  def push[A](stack : Stack[A], value : A) : Stack[A] = Stk(value, stack)
  
  def pop[A](stack : Stack[A]) : Option[A] = {
    stack match {
      case Empty => None
      case Stk(h,st) => Some(h)
    }
  }

  def tail[A](stack : Stack[A]) : Stack[A] = {
    stack match {
      case Empty => Empty
      case Stk(h,st) => st
    }
  }
}


case class MinStack[A](values : Stack[A], minima : Stack[A])
// If we instead say MinStack[A : Ordering]...
// we cannot make an empty MinStack because there is no implicit evidence that Nothing extends Ordering
// If, instead, we put the type constraints on the functions, we are okay


def push[A](mstack : MinStack[A], value : A)(implicit ev : Ordering[A]) : MinStack[A] = {
  mstack match {
    case MinStack(Empty, _) => MinStack(Stk(value, Empty), Stk(value, Empty))
    case MinStack(_, Empty) => MinStack(Stk(value, Empty), Stk(value, Empty))
    case MinStack(vals, mins) if ev.lteq(value, Stack.pop(mins).get) => MinStack(Stack.push(vals, value), Stack.push(mins, value))
    case MinStack(vals, mins) => MinStack(Stack.push(vals,value), mins)
  }
}

def fromList[A](ls : List[A])(implicit ev : Ordering[A]) : MinStack[A] = {
  ls.foldLeft[MinStack[A]](MinStack(Empty,Empty)){case (accum, elem) => push(accum, elem)}
}

val ms = fromList(List(1,6,4,23,5,54,3,2))
val mms = fromList("a man a plan a canal panama".split(" ").toList)

def pop[A](mstack : MinStack[A])(implicit ev : Ordering[A]) : Option[A] = Stack.pop(mstack.values)

def min[A](mstack : MinStack[A])(implicit ev : Ordering[A]) : Option[A] = Stack.pop(mstack.minima)

def tail[A](mstack : MinStack[A])(implicit ev : Ordering[A]) : MinStack[A] = {
  mstack match {
    case MinStack(Empty, _) => MinStack(Empty, Empty)
    case MinStack(_, Empty) => MinStack(Empty, Empty)
    case MinStack(values, minima) if Stack.pop(values) == Stack.pop(minima) => MinStack(Stack.tail(values), Stack.tail(minima))
    case MinStack(values, minima) => MinStack(Stack.tail(values), minima)
  }
}


