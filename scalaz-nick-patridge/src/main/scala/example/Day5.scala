package example
import cats._
import cats._
import cats.data.Kleisli
import cats.syntax.all._


object Day5 extends App {

  "wisdom".some map { _ + "1"}

  type Birds = Int
  case class Pole(left: Birds, right: Birds){
    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[Pole]
    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[Pole]
    def banana: Option[Pole] = none[Pole]
  }

  val lbl = Monad[Option].pure(Pole(0, 0)) >>= {_.landLeft(1)} >>=
    {_.banana} >>= {_.landRight(1)}

  // Instead of making functions that ignore their input and just return a predetermined monadic value, we can use the >> function
  // Here's how >> behave with Option:

  none[Int] >> 3.some

  3.some >>= { x => "!".some >>= { y => (x.show + y).some } }

}



