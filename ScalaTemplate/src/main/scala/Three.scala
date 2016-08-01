package scalatemplate

object Three {

  /**
  * Write one function that works for multiple datatypes using type classes!
  * Let's make some monoids!
  * https://en.wikipedia.org/wiki/Monoid
  * Confession: we actually only need a Semigroup for this example, we never use the zero element
  * but monoids are pretty common so let's just go with it!
  */
  trait Monoid[A] {
    def zero : A
    def plus(x : A, y : A) : A
  }
    
  implicit object IntMonoid extends Monoid[Int] {
    def zero : Int = 0
    def plus(x : Int, y : Int) : Int = x + y
  }

  implicit object StringMonoid extends Monoid[String] {
    def zero : String = ""
    def plus(x : String, y : String) : String = x + y
  }

  /**
  * Write a generic function that works for any type A for which we know how to add things
  */
  def double[A : Monoid](x : A) : A = {
    // use implicit evidence that A is a monoid
    val m = implicitly[Monoid[A]]
    m.plus(x,x)
  }
}