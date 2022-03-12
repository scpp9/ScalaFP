package example
import cats.free.Free
import cats.syntax._
object Trampoline extends App {

  sealed trait Trampoline[+A] {

    final def resume: Either[()=> Trampoline[A], A] = this match {
      case Done(v) => Right(v)
      case More(k) => Left(k)
      case FlatMap(a, f) => a match {
        case Done(v)=> f(v).resume
        case More(k) => Left(() => FlatMap(k(), f))
        case FlatMap(b, g) => FlatMap(b, ((x:Any)=> FlatMap(g(x), f): Trampoline[A])).resume
      }
    }

   final def runT: A = resume match {
     case Right(v) => v
     case Left(k) => k().runT
   }


    def map[B](f: A => B) : Trampoline[B] = flatMap(x => More(() => Done(f(x))))
    def flatMap[B](f: A=> Trampoline[B]): Trampoline[B] = FlatMap(this, f)
  }

  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
  case class Done[+A](result: A) extends Trampoline[A]
  case class FlatMap[A, B](a: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def even[A](ns: List[A]) : Trampoline[Boolean] = ns match {
    case Nil => Done(true)
    case x::xs => More(() => odd(xs))
  }

  def odd[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(false)
    case x::xs => More(()=>even(xs))
  }

  print(even(Range(0, 1000000).toList))



  case class State[S, +A](runS: S => Trampoline[(A, S)]) {

    def map[B](f: A=> B) = State[S, B](s => { runS(s) map { case (a, s1) => (f(a), s1)} })

    def flatMap[B](f: A=> State[S, B]) = State[S, B](s => More(() => runS(s) flatMap {case (a, s1) => More(() => f(a) runS s1)}))

  }

 type Pair[A] = (A, A)
  type BinTree[+A] = Free[Pair, A]

 type Tree[+A] = Free[List, A]
// type FreeMonoid[A] = Free[({type LMDA[ALPHA] = (ALPHA, A)})#LMDA, Unit]

 //Let's try defining "List" using Free
 type FreeMonoid[A] = Free[(A, *), Unit]

  def cons[A](a: A): FreeMonoid[A] = Free.liftF((a,()))

  val x = cons(1)
}
