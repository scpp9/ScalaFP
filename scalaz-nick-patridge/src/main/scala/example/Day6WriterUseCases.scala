package example

import cats.data.Writer

object  Day6WriterUseCases extends App{
    val l = Writer.tell("Log something")
    print(l.run)

}
