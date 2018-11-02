package org.saegesser

import scala.math.Integral.Implicits._

object Day3 {
  val RightSide  = 0
  val TopSide    = 1
  val LeftSide   = 2
  val BottomSide = 3

  case class Coord(r: Int, c: Int) {
    def +(other: Coord): Coord = Coord(r+other.r, c+other.c)
  }

  def ringSize(r: Int): Int =
    if(r == 0) 1
    else      r*8

  def ringBase(r: Int): Int =
    (0 to r-1).map(ringSize).sum

  def indexToRingAndOffset(idx: Int): (Int, Int) = {
    val (r, b) = Stream.from(0).map(r => (r, ringBase(r))).takeWhile(_._2 < idx).last
    (r, idx - b - 1)
  }

  def ringAndOffsetToAddr(r: Int, o: Int): Int = {
    ringBase(r) + o
  }

  def indexToCoord(idx: Int): Coord = {
    if(idx == 1) Coord(0, 0)
    else {
      val (ring, offset) = indexToRingAndOffset(idx)
      val sideLength     = ring * 2
      val (side, cell)   = offset /% sideLength
      val startCell      = ring - 1

      side match {
        case RightSide  => Coord(cell - startCell, ring)
        case TopSide    => Coord(ring            , startCell - cell)
        case LeftSide   => Coord(startCell - cell, -ring)
        case BottomSide => Coord(-ring           , cell - startCell)
      }
    }
  }

  def coordToIndex(coord: Coord): Int = {
    val ring       = math.max(math.abs(coord.r), math.abs(coord.c))
    val sideLength = ring * 2
    val startCell  = ring - 1

    val offset =
      coord match {
        case Coord(0, 0)              => 0
        case Coord(r, c) if r == -ring => c + startCell + 3*sideLength  // BottomSide
        case Coord(r, c) if c == ring  => r + startCell                 // RightSide
        case Coord(r, c) if r == ring  => startCell - c + sideLength    // TopSide
        case Coord(r, c) if c == -ring => startCell - r + 2*sideLength  // LeftSide
      }

    ringBase(ring) + offset + 1
  }

  def stepsForIndex(idx: Int): Int = {
    val c = indexToCoord(idx)
    math.abs(c.r) + math.abs(c.c)
  }

  val adjacentCells = List(Coord(-1, -1), Coord(-1, 0), Coord(-1, 1), Coord(0, -1), Coord(0, 1), Coord(1, -1), Coord(1, 0), Coord(1, 1))

  def adjacencies(coord: Coord): List[Coord] = {
    val currIndex = coordToIndex(coord)
    adjacentCells
      .map(c => coord + c)
      .map(c => (coordToIndex(c), c))
      .filter(_._1 < currIndex)
      .map(_._2)
  }

  def stressTestPuzzle(value: Int): Int ={
    def helper(v: Vector[Int], i: Int): Int = {
      val nextValue =
        adjacencies(indexToCoord(i))      // Get available adjacent cells
          .map(c => v(coordToIndex(c)))   // Get the values for those cells
          .sum                            // Add them all together
      if(nextValue > value) nextValue
      else                  helper(v :+ nextValue, i+1)
    }

    helper(Vector(0, 1), 2)
  }

  def nextValue(v: Vector[Int], i: Int): Int =
    adjacencies(indexToCoord(i))
      .map(c => v(coordToIndex(c)))
      .sum

  val puzzleData = 347991
}
