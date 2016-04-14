//
// You have two numbers represented as linked lists, where each node contains a single digit. 
// The digits are stored in reverse order, such that the 1’s digit is at the head of the list. 
// Write a function that adds the two numbers and returns the sum as a linked list.
// 
// EXAMPLE
// Input: (3 -> 1 -> 5) + (5 -> 9 -> 2)
// Output: 8 -> 0 -> 8
//
case class Lint(digits : List[Int]) {require (digits.map(_ / 10).sum == 0)}
// Lintegers, integers as lists of single digits
//
// Pimp My Monoid
// idea: 
//  - define bijections Lint => Int and Int => Lint, 
//  - define addition of Lintegers in terms of addition of Integers.
// In math terms: we are pulling back the monoid structure on Integers to Lintegers.
// Moral of the story: for any cool thing C, given a not-cool thing T and a bijection f : T => C,
// we can make T cool by pulling back the cool structure on C via f.
// Algebraically speaking: we can always make T a <cool algebraic structure on C> 
// by choosing the <cool algebraic structure> that makes f a homomorphism.

def toInt(lint : Lint) : Int = {
  val digitsWithPowers = lint.digits.zipWithIndex
  digitsWithPowers
    .map{case (digit, power) => digit * Math.pow(10, power).toInt}
    .sum
}

def toLint(int : Int) : Lint = {
  val digits = 
    int
      .toString
      .split("")
      .reverse
      .map(x => x.toInt)
      .toList
  Lint(digits)
}

object Monoidies {
  
  trait Monoid[A] {
    val zero : A
    def plus(x : A, y : A) : A
  }

  implicit object LintMonoid extends Monoid[Lint] {
    val zero = Lint(List(0))
    def plus(x : Lint, y : Lint) : Lint = toLint(toInt(x) + toInt(y))
  }
  
  implicit object IntMonoid extends Monoid[Int] {
    val zero = 0
    def plus(x : Int, y : Int) : Int = x + y
  }

  def plus[A](x : A, y : A)(implicit ev : Monoid[A]) : A = ev.plus(x,y)
 
}


import Monoidies._

val a = 34521
val b = 167
val sumInt = plus(a,b)

val aLint = toLint(a)
val bLint = toLint(b)
val sumLint = plus(aLint,bLint)

val agree = sumInt == toInt(sumLint)
