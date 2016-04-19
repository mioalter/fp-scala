package interpreters

object Exp {

  import language.higherKinds
  import UsefulTraits._

  sealed trait Exp[T]
  final case class Const[T](s : T) extends Exp[T]
  final case class Add[T](a : T, b : T) extends Exp[T]	

  implicit object ExpFunctorInstance extends Functor[Exp] {
    def map[T,U](e : Exp[T])(f : T => U) : Exp[U] = 
      e match {
        case Const(s) => Const(f(s))
        case Add(a,b) => Add(f(a),f(b))
      }    
  }

}