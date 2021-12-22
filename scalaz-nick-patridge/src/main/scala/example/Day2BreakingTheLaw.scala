package  example
import cats._

sealed trait CoOption[+A]
case class CSome[A](counter: Int, a: A) extends  CoOption[A]
case object Cnone extends  CoOption[Nothing]

object CoOption {

  implicit def coOption[A] : Eq[CoOption[A]] = new Eq[CoOption[A]] {
    override def eqv(x: CoOption[A], y: CoOption[A]): Boolean = x == y
  }

  implicit val coOptionFunctor = new Functor[CoOption] {
    override def map[A, B](fa: CoOption[A])(f: A => B): CoOption[B] =
      fa match {
        case Cnone => Cnone
        case CSome(c, a) => CSome(c+1, f(a))
      }
  }
}
