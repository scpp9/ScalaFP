package example

class FreeMonads {
   sealed trait CharToy[+Next]

   object charToy {
       case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
       case class CharBell[Next](next: Next) extends  CharToy[Next]
       case class CharDone() extends CharToy[Nothing]
       def done: CharToy[Nothing] = CharDone()
}
{
   import charToy._
   CharOutput('A', done)
}
 case class Fix[F[_]](f: F[Fix[F]])

  object Fix {
    def fix(toy: CharToy[Fix[CharToy]]) = Fix[CharToy](toy)
  }

 // FixE
 // We are also going to try to implement FixE, which adds an exception to this. Since throw and catch are reserved
 // I am renaming to throwy and catchy
 sealed trait FixE[F[_], E]
 object FixE {
   case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
   case class Throwy[F[_], E](e: E) extends  FixE[F, E]




 }

  trait Monad[M[_]]{
    def pure[A](a: A): M[A]
    def flatMap[A, B](a: M[A])(fn: A => M[B] ): M[B]
    def map[A, B](a: M[A])(f: A=>B): M[B] = flatMap(a){b => pure(f(b)) }
  }


}