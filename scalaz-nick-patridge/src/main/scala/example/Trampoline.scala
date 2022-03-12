package example

object Trampoline extends App {

  sealed trait Trampoline[+A] {
    final def runT: A = this match {
      case More(k) => k().runT
      case Done(v) => v
    }

    def map[B](f: A => B) : Trampoline[B] = flatMap(x => More(() => Done(f(x))))

    def flatMap[B](f: A=> Trampoline[B]): Trampoline[B] = More(()=> f(runT))
  }

  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
  case class Done[+A](result: A) extends Trampoline[A]

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



}
