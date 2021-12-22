package example

import cats.Applicative
import cats.implicits.{catsSyntaxApply, catsSyntaxOptionId}
import cats._
import cats.syntax.all._


object Day4 extends App {
  //https://www.youtube.com/watch?v=T_0IE8PF1sY&t=1459s

  def add(a: Int, b: Int) = a+ b

  Applicative[Option].pure((a:Int) => (b: Int) => add(a, b)).ap(1.some).ap(2.some)

  println(List(1,2,3).foldMap((_:Int) * 3))

  println(List(1,2,3).foldMap(identity))

}
