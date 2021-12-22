object Tst
{
trait Monoid[A] {
  def mappend(a: A, b: A): A
  def mzero: A
}
object Monoid {

  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

implicit class MonoidOps[A: Monoid](lhs: A) {
  def |+|(rhs: A): A = implicitly[Monoid[A]].mappend(lhs, rhs)
}

  def main(args: Array[String]): Unit = {
    println(2 |+| 3)
  }


}
