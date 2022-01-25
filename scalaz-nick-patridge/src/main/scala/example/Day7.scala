package example


class Day7 {

  type Stack = List[Int]

  import cats._, cats.data._, cats.syntax.all._

  val pop = State[Stack, Int] {
    case x :: xs => (xs, x)
    case Nil => sys.error("stack is empty")
  }

  def push(a: Int) = State[Stack,Unit] {
    case xs => (a :: xs, ())
  }

  def stackManip: State[Stack, Int] = for {
    _ <- push(3)
    a <- pop
    b <- pop
  } yield(b)

  stackManip.run(List(5,8,2,1)).value

}
