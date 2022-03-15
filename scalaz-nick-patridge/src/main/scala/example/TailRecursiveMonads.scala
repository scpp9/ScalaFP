package example

import cats.FlatMap
import cats.data.{Writer, WriterT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.kernel.Monoid

import java.lang.System.Logger

object TailRecursiveMonads extends App {

     case class LongProduct(value: Long)

     implicit val longProductMon: Monoid[LongProduct] = new Monoid[LongProduct] {
       override def empty: LongProduct = LongProduct(1)
       override def combine(x: LongProduct, y: LongProduct): LongProduct = LongProduct(x.value * y.value)
     }

     def powWriter(x: Long, exp: Long): Writer[LongProduct, Unit] = exp match {
       case 0 => Writer(LongProduct(1), ())
       case _ => Writer(LongProduct(x), ()) >>= { _ => powWriter(x, (exp-1))}
     }

     def powWriter2(x: Long, exp: Long): Writer[LongProduct, Unit] = FlatMap[Writer[LongProduct, *]].tailRecM(exp) {
       case 0L => Writer.value[LongProduct, Either[Long, Unit]](Right(()))
       case m: Long => Writer.tell(LongProduct(x)) >>= { _ => Writer.value(Left(m - 1)) }
     }


   // https://medium.com/@alexander.zaidel/stack-safe-monads-33e803065a9d







  println(List(1,2,3) >>= { x => List(x , x + 1)})
     print(powWriter(2,3))

     print(powWriter2(1, 10000))
}
