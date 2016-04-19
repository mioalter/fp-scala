package interpreters

object Implicits {

  import UsefulTraits._

  implicit object IntMonoid extends Monoid[Int] {
    def zero : Int = 0
    def plus(a : Int, b : Int) : Int = a + b
  }

  implicit object StringMonoid extends Monoid[String] {
  	def zero : String = ""
  	def plus(a : String, b : String) : String = a ++ b
  }

}