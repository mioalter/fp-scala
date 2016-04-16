package interpreters

object BoolAlg {
  
  import language.higherKinds
  import UsefulTraits._

  sealed trait BoolAlg[A]
  final case class Lit[A](b : Boolean) extends BoolAlg[A]
  final case class And[A](a : A, b : A) extends BoolAlg[A]
  final case class Or[A](a : A, b : A) extends BoolAlg[A]
  final case class Not[A](a : A) extends BoolAlg[A]
}
