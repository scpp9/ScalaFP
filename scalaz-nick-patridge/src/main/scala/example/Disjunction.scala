package example
import cats.{Eq, Monoid}
import cats.syntax._
import cats.implicits._

class Disjunction(val unWrap: Boolean) {

  object Disjunction {
    def apply(a: Boolean): Disjunction =  Disjunction(a)
    implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {

      override def combine(x: Disjunction, y: Disjunction): Disjunction = Disjunction(x.unWrap || y.unWrap)

      override def empty: Disjunction = Disjunction(false)
    }

    implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
      override def eqv(x: Disjunction, y: Disjunction): Boolean = x.unWrap == y.unWrap
    }
  }


}
