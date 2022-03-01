package example
import cats._, cats.data._, cats.syntax.all._
import cats.data.Validated.{valid, invalid}
import cats.data.{ NonEmptyList => NEL}

object  Day7Validated extends App {

  val result = (valid[String, String]("event 1 ok"),
    invalid[String, String]("event 2 failed!"),
    invalid[String, String]("event 3 failed!")) mapN {_ + _ + _}

  print(result)

  // IOR


  // As noted in the scaladoc comment, Ior's flapMap uses Semigroup[A] to accumulate
  // failures when it sees Ior.both(...) value. So we could probably
  // use this as a hybrid of Xor and Validated

  // Here's how flatMap behaves for all nine combinations

 val c1 = Ior.right[NEL[String], Int](1) >>= {x => Ior.right[NEL[String], Int](x+1)}
 print(c1)

 val c2 = Ior.left[NEL[String], Int](NEL.of("error 1")) >>=  {x => Ior.right[NEL[String], Int](x+1)}
 print(c2)



}
