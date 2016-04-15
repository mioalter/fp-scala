package interpreters

object Implicits {

  //import language.higherKinds
  import UsefulTraits._

  implicit object IntMonoid extends Monoid[Int] {
    def zero : Int = 0
    def plus(a : Int, b : Int) = a + b
  }

}