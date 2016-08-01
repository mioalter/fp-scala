package scalatemplate

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalatest.{Matchers, FunSuite}
import scalatemplate.One._
import scalatemplate.Two._
import scalatemplate.Three._

/**
* Tests to check specific inputs
*/
class ScalaTemplate_Test extends FunSuite with Matchers {

  test("doubleString doubles a String test") {
    val result = doubleString("Mio")
    val expected = "Mio" + "Mio"
    result should be (expected)
  }

  test("doubleInt doubles an Int test") {
    val result = doubleInt(42)
    val expected = 42 + 42
    result should be (expected)
  }

  test("double doubles Strings test") {
    val result = double("Mio")
    val expected = "Mio" + "Mio"
    result should be (expected)
  }

  test("double also doubles Ints test") {
    val result = double(42)
    val expected = 42 + 42
    result should be (expected)
  }
  
}

/**
* Properties to check that general statements about our program hold for all automatically generated inputs
*/
object ScalaTemplate_Prop extends Properties("ScalaTemplate") {
  
  property("doubleString doubles a String property") = 
    forAll{s : String => 
      val result = doubleString(s)
      val expected = s + s
      result == expected
    }

  property("doubleInt doubles an Int property") = 
    forAll{i : Int =>
      val result = doubleInt(i)
      val expected = i + i
      result == expected
    }

  property("double doubles Strings property") =
    forAll{s : String =>
      val result = double(s)
      val expected = s + s
      result == expected
    }

  property("double also doubles Ints property") = 
    forAll{i : Int =>
      val result = double(i)
      val expected = i + i
      result == expected
    }
}
