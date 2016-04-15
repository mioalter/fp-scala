package interpreters

object Identity {

  import language.higherKinds
  import UsefulTraits._
  
  type Id[A] = A 
  // avoid lots of wrapping and unwrapping
  // this is a type synonym so Id[A] and A are interchangeable

  implicit object IdentityMonadInstance extends Monad[Id] {
    def map[A,B](ida : Id[A])(f : A => B) : Id[B] = f(ida)
    def pure[A](a : A) : Id[A] = a
    def bind[A,B](ida : Id[A])(f : A => Id[B]) : Id[B] = f(ida)
  }

}