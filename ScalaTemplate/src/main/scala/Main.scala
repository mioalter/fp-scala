package scalatemplate

import scalatemplate.One._
import scalatemplate.Two._
import scalatemplate.Three._

object Main extends App {

  val name = args(0)

  /**
  * Once, the easy way
  */
  val doubleLength = doubleInt(name.length)
  val doubleName = doubleString(name)

  println(s"Hello, $name!")
  println(s"your name twice is $doubleName")
  println(s"the length of your name twice is $doubleLength")

  /**
  * Once again, using type classes!
  */

  val doubleLengthGeneric = double(name.length)
  val doubleNameGeneric = double(name)
  println(s"Hello again, $name!")
  println(s"your name twice is $doubleNameGeneric")
  println(s"the length of your name twice is $doubleLengthGeneric")
}
