package example

import cats.data.Writer
import cats.kernel.Semigroup
import cats.syntax.all._

import scala.collection.BitSet

object Day6 extends App {

  val bits = BitSet(1,2,3)

  implicit class PairOps[A, B: Semigroup](pair: (A, B)){
    def applyLog[C](f : A => (C, B)) : (C, B) ={
      val (x, log) = pair
      val (y, newlog) = f(x)
      (y, log |+| newlog)
    }
  }

  val w = Writer("Smallish gang.", 3)
  print(w.run)



}
