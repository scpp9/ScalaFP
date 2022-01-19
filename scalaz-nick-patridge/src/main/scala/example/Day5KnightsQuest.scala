package example

case class Day5KnightsQuest(c: Int, r: Int) extends App {
  def move: List[Day5KnightsQuest] =
    for {
      Day5KnightsQuest(c2, r2) <- List(Day5KnightsQuest(c+2, r+1), Day5KnightsQuest(c+2, r-1)
      , Day5KnightsQuest(c-2, r+1), Day5KnightsQuest(c-2, r-1), Day5KnightsQuest(c+1, r-2), Day5KnightsQuest(c+1, r-2), Day5KnightsQuest(c-1, r-2), Day5KnightsQuest(c-1, r+2) )
       if(((1 to 8).toList contains c2) && ((1 to 8).toList contains r2))
    } yield Day5KnightsQuest(c2,r2)

  def in3 : List[Day5KnightsQuest] =
    for {
      first <- move
      second <- first.move
      third <- second.move
    } yield third

  def canReachIn3(end:  Day5KnightsQuest): Boolean = in3 contains end

  print(Day5KnightsQuest(6,2) canReachIn3 Day5KnightsQuest(6, 1) )
}

