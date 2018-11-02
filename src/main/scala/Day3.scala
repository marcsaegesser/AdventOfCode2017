package org.saegesser

object Day3 {
  case class PuzzleState(idx: Int, length: Int, ringNumber: Int, increment: Int, side: Int)
  val RightSide  = 1
  val TopSide    = 2
  val LeftSide   = 3
  val BottomSide = 4
  val baseState = PuzzleState(1, 0, 0, +1, BottomSide)

  def nextState(curr: PuzzleState): PuzzleState = {
    @inline def maxLengthForRing(r: Int): Int = r*2

    if(curr.length == maxLengthForRing(curr.ringNumber))
      if(curr.side < BottomSide) curr.copy(idx = curr.idx+1, length = curr.length-1, increment = -1, side = curr.side+1)
      else                       curr.copy(idx = curr.idx+1, length = curr.length+1, ringNumber = curr.ringNumber+1, increment = -1, side = RightSide)
    else if(curr.length == curr.ringNumber) curr.copy(idx = curr.idx+1, length = curr.length+1, increment = +1)
    else                                   curr.copy(idx = curr.idx+1, length = curr.length+curr.increment)
  }

  def stepsForIndex(idx: Int): Int = {
    val stateIter = new Iterator[PuzzleState] {
      var curr = baseState
      def hasNext = true
      def next() = {
        curr = nextState(curr)
        curr
      }
    }

    stateIter.dropWhile(s => s.idx < idx).next().length
  }

  val puzzleData = 347991
}
