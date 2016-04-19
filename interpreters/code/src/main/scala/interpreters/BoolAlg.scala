package interpreters

object BoolAlg {
  
  import language.higherKinds
  import UsefulTraits._

  sealed trait BoolAlg[A]
  final case class Lit[A](b : Boolean) extends BoolAlg[A]
  final case class And[A](a : A, b : A) extends BoolAlg[A]
  final case class Or[A](a : A, b : A) extends BoolAlg[A]
  final case class Not[A](a : A) extends BoolAlg[A]

  implicit object BoolAlgFunctor extends Functor[BoolAlg] {
  	def map[A,B](ba : BoolAlg[A])(f : A => B) : BoolAlg[B] =
  	  ba match {
  	  	case Lit(b) => Lit(b)
  	  	case And(a,b) => And(f(a),f(b))
  	  	case Or(a,b) => Or(f(a),f(b))
  	  	case Not(a) => Not(f(a))
  	  }
  }
}
