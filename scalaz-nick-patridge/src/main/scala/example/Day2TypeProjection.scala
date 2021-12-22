package example

class Day2TypeProjection {
  trait Access[Res[_]] {
    def access[C] : Res[C]
  }

  trait CList[C1, A] extends Access[({type LAMBDA[X] = CList[X, A]})#LAMBDA]
  }

