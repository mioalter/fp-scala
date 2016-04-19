package interpreters

object Free {

  import language.higherKinds
  import UsefulTraits._
  import Implicits._
  
  sealed trait Free[F[_],A]{
    
    def map[B](f : A => B)(implicit ev : Functor[F]) : Free[F,B] = 
      this match {
        case Return(a) => Return(f(a))
        case Suspend(s) => Suspend(ev.map(s)(_.map(f))) 
        // s = F[y] for some y : Free[F,A]
        // By our recursive construction, Free[F,A].map(f) knows how to eat y
        // then we F.map it over s = F[y]
      }
    
    def bind[B](f : A => Free[F,B])(implicit ev : Functor[F]) : Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Suspend(s) => Suspend(ev.map(s)(_.bind(f)))
      }
    
    def foldMap[G[_]](f : F ~> G)(implicit ev : Monad[G]) : G[A] = 
      this match {
        case Return(a) => ev.pure(a)
        case Suspend(s) => ev.bind(f(s))(_.foldMap(f)) // f(s) >>= foldMap(f) in Haskell 
      }

  }
  
  case class Return[F[_],A](a : A) extends Free[F,A]
  case class Suspend[F[_],A](s : F[Free[F,A]]) extends Free[F,A]

}