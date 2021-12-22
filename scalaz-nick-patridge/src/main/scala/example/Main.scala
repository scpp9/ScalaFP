
trait FoldLeft[F[_]] {
   def foldLeft[A, B](xs: F[A], b:B, f: (B,A) => B) : B
}

object  FoldLeft {
  implicit object foldLeftList extends  FoldLeft[List]{
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}

trait  Monoid[A] {
  def append(a: A, b: A): A
  def zero: A
}

object  Monoid {

 implicit object IntMonoid extends Monoid[Int] {
    def append(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  implicit  object  StringMonoid extends Monoid[String] {
    override def append(a: String, b: String): String = a + b
    override def zero: String = ""
  }

}

trait Identity[A] {
  val value: A
  def plus(a2: A)(implicit m: Monoid[A]) : A = m.append(value, a2)
}

trait MA[M[_], A] {
  val value: M[A]

  def sum(implicit m: Monoid[A], f1: FoldLeft[M]) : A = f1.foldLeft(value, m.zero, m.append)
}

object Main {

  implicit  def toIdent[A](a: A): Identity[A] = new Identity[A] {
    override val value = a
  }

  implicit  def toMA[M[_], A](ma: M[A]) : MA[M, A] = new MA[M, A] {
    override val value: M[A] = ma
  }



  val multMonoid = new Monoid[Int] {
    override def append(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }

 def sum[M[_], T](xs: M[T])(implicit m: Monoid[T], f1: FoldLeft[M]): T = f1.foldLeft(xs, m.zero, m.append)

  def plus[T](a: T, b: T)(implicit m: Monoid[T]) : T = m.append(a, b)

  def p(a: Any){println("###> " + a)}

  def main(args: Array[String]): Unit = {
    println

    p(sum(List(1,2,3,4)))
    p(sum(List(1,2,3,4))(multMonoid, implicitly[FoldLeft[List]]))
    p(sum(List("a","b","c")))
    p(plus(3, 4))
    p(3.plus(4))
    println
  }
}
