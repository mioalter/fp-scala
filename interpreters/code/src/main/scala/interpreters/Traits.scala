package interpreters

object UsefulTraits {

  import language.higherKinds

  trait Monoid[A] {
    def zero : A
    def plus(a : A, b : A) : A
  }

  trait Functor[F[_]] {
    def map[A,B](fa : F[A])(f : A => B) : F[B]
  }  

  trait Monad[M[_]] extends Functor[M] {
    def pure[A](a : A) : M[A]
    def bind[A,B](ma : M[A])(f : A => M[B]) : M[B]
  }

  trait ~>[F[_],G[_]] {
    def apply[A](fa : F[A]) : G[A]
  }

}