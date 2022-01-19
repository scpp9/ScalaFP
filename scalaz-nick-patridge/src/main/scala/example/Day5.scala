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

  def routine: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
//      third <- second.landLeft(1)
    } yield second

  print(routine)

// So in this monadic view, a List context represents a mathematical value that could have multiple solutions
// Other than that manipulating Lists using for notation is just like plain Scala:
  // FunctorFilter
  // Scala's for comprehension allows filtering:

val english = Map(1 -> "one", 3 -> "three", 10 -> "ten")

(1 to 50).toList mapFilter(english.get(_))

def collectEnglish[F[_]: FunctorFilter](f: F[Int]): F[String] =
  f collect {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
  }

  collectEnglish((1 to 50).toList)

}



