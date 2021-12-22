package example

import cats.laws.discipline.{ FunctorTests }
import org.scalacheck.{ Arbitrary, Gen}

class COptionTest extends munit.DisciplineSuite {
  checkAll("COption[Int]", FunctorTests[CoOption].functor[Int, Int, Int])

  implicit def coptionArbiterery[A](implicit arbA: Arbitrary[A]) : Arbitrary[CoOption[A]]  = Arbitrary {
    val arbSome = for {
         i <- implicitly[Arbitrary[Int]].arbitrary
         a <- arbA.arbitrary
      } yield (CSome(i, a): CoOption[A])
    val arbNone = Gen.const(Cnone: CoOption[Nothing])
    Gen.oneOf(arbSome, arbNone)
  }
}
