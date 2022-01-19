package example


class Day7 {

  type stack = List[Int]

  def pop(s0: stack): (stack, Int) = s0 match {
    case x:: xs => (xs, x)
    case Nil => sys.error("stack is empty")
  }



}
