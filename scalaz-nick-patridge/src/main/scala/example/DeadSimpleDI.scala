package example


case class DeadSimpleDI[A](g: Int => A) {

  implicit def Con[B](f: Int => B) = new DeadSimpleDI[B](f)

  def apply(c: Int) = g(c)
  def map[B](f: A=> B): DeadSimpleDI[B] =
    (c:Int) => f(g(c))

  def flatMap[B](f: A=> DeadSimpleDI[B]) : DeadSimpleDI[B] =
  (c:Int) => f(g(c))(c)

}
