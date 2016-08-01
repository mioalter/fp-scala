
object Tutorial {
  
  /**
  * Vars and Vals
  */
  
  var x = 3 // mutable, later can say var x = 4
  val immutableList = List(1,2,3) //immutable
  val maxElement = immutableList.sorted.reverse.head // get largest element
  
  /**
  * Notice, everything has a type (the compiler can infer types, but not always)
  */
  
  /**
  * Functions
  */
  
  def square(x : Int) : Int = x * x
  
  def greet(name : String) : String = s"Hello, $name!"
  
  /**
  * Functions are values
  */
  
  val squarer : (Int => Int) = x => x * x
  
  /**
  * Partial function application
  */
  
  def add(x : Int, y : Int) : Int = x + y
  def addOne(y : Int) : Int = add(1,y)
  def addOnePrime : (Int => Int) = add(1,_)
  
  def adder(x : Int)(y : Int) : Int = x + y
  def adderOne : (Int => Int) = adder(1) _
  
  /**
  * Pattern matching
  */
  
  def sum(alist : List[Int]) : Int = 
    alist match {
      case Nil => 0
      case (x :: xs) => x + sum(xs)
    }
  
  def sumEvens(alist : List[Int]) : Int = 
    alist match {
      case Nil => 0
      case (x :: xs) if x % 2 == 0 => x + sumEvens(xs)
      case (x :: xs) => sumEvens(xs)
    }
  
  /**
  * Map, Filter, Fold
  * Ex 1
  */
  
  val l = List(1,6,4,7,8,9,4,3,2,5,6)
  
  val squared = l.map(square) 
  // list.map(f) with () if we are just passing the function
  val squared2 = l.map{case element => square(element)} 
  //list.map{case x => f(x)} if we are saying what the function does
  
  val evenSquares = squared.filter{case x => x % 2 == 0}
  
  val sumOfEvenSquares = evenSquares.sum
  
  val sumOfEvenSquares2 = l.map(square).filter{case x => x % 2 == 0}.sum
  
  val sumEvenSquares3 = l.map(square).filter{case x => x % 2 == 0}.foldLeft(0){case (accum, elem) => accum + elem}
  
  /**
  * Fold has type (B, (B,A) => B) => B
  * That is, to call it on a list of As, you have to pass it 2 arguments:
  * - the initial value of the accumulator
  * - a function of type (B,A) => B that says how to combine next element (of type A) with the current value of the accumulator (of type B) to make a new accumulator (of type B)
  * Fold goes down the input list of type A, starts with the initial accumulator, and applies the function to combine the next element with the accumulator
  * then stops when it gets to the end of the list
  */
  
  /**
  * Map, Filter, Fold
  * Ex 2
  */
  
  /**
  * A list of functions! (You can easily compose functions!)
  */
  val someFunctions = List(squarer, squarer compose squarer, squarer compose squarer compose squarer)
  
  def evaluate(functions : List[Int => Int], value : Int) : List[Int] = functions.map{case f => f(value)}
  
  val valuesAt3 = evaluate(someFunctions, 3)
  
  /**
  * More list operations
  */
  
  val alist = List("a","b","c","d")
  
  // :: ("cons")
  val prependZ = "z" :: alist
  
  // append
  val appendZ = alist :+ "z"
  
  // add lists
  val mirrorList = alist ++ alist.reverse
  
  // zip
  val pairs = alist.zip(List(4,2,3,1))
  
  // zipWithIndex
  val elementsWithIndices = alist.zipWithIndex
  
  // head
  val firstElement = alist.head
  
  // headOption
  val maybeFirstElement = alist.headOption
  val noHead = List().headOption
  // This way, if there is no value, it returns None instead of throwing an error.
  // You can handle the None later in your code however you want instead of crashing your program
  
  // take
  val first2 = alist.take(2)
  
  // drop
  val last2 = alist.drop(alist.length - 2)
  
  // takeWhile
  val upToD = mirrorList.takeWhile{case x => x < "d"}
  
  // dropWhile
  val afterC = mirrorList.dropWhile{case x => x < "d"}
  
  // For comprehension
  // Easy to do multiply-nested with only one level of nesting
  val rainbow = "rainbow".toList
  val tripleRainbow = for {
    a <- rainbow
    b <- rainbow
    c <- rainbow
    if (a < b) && (b < c)
  } yield (a,b,c)
  
  /**
  * So, lists are like For loops.
  */
  
  
  
  /**
  * Laziness
  */
  
  def slowFibs(n : Int) : Int = 
    n match {
      case n if n <= 2 => 1
      case _ => slowFibs(n - 1) + slowFibs(n - 2)
    }
  
  object Fibs {
    val fibs : Stream[BigInt] = 1 #:: 1 #:: fibs.zip(fibs.drop(1)).map{case (x,y) => x + y}
  }
  
  def fastFibs(n : Int) : BigInt =
    n match {
      case n if n < 1 => 0
      case _ => Fibs.fibs.take(n).drop(n-1).head
    }
  
  
  /**
  * Won't evaluate until you ask for it
  */
  lazy val slow45th = slowFibs(45)
  
  /**
  * Streams are lazy lists, won't evaluate more than you ask for, caches already evaluated parts so doesn't recompute
  */
  lazy val slows = Stream(45, 46, 47, 48, 49, 50).map{case n => slowFibs(n)}

}
