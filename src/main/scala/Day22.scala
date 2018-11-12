package org.saegesser

object Day22 {
  type Grid = Map[(Int, Int), Condition]
  type Position = (Int, Int)

  trait Condition
  case object Clean    extends Condition
  case object Weakened extends Condition
  case object Infected extends Condition
  case object Flagged  extends Condition


  type Direction = Int
  val Right:Direction = 0
  val Up:Direction    = 1
  val Left:Direction  = 2
  val Down:Direction  = 3
  val NumDirections: Int = 4

  case class State(g: Grid, p: Position, d: Direction, count: Int)

  def iterate(state: State): State =
    state match { case State(g, p, d, count) =>
      getCondition(g,p) match {
        case Clean    =>
          val newD = turnLeft(d)
          State(weakenNode(g, p), move(p, newD), newD, count)
        case Weakened =>
          State(infectNode(g, p), move(p, d), d, count+1)
        case Infected =>
          val newD = turnRight(d)
          State(flagNode(g, p), move(p, newD), newD, count)
        case Flagged  =>
          val newD = reverse(d)
          State(cleanNode(g, p), move(p, newD), newD, count)
      }
    }

  def iterateN(state: State, n: Int): State =
    if(n == 0) state
    else      iterateN(iterate(state), n-1)

  def turnLeft(d: Direction): Direction = (d+1)%NumDirections

  def turnRight(d: Direction): Direction = (d-1+NumDirections)%NumDirections

  def reverse(d: Direction): Direction = (d+NumDirections/2)%NumDirections

  def move(p: Position, d: Direction): Position =
    d match {
      case Right => (p._1  , p._2+1)
      case Up    => (p._1-1, p._2)
      case Left  => (p._1  , p._2-1)
      case Down  => (p._1+1, p._2)
    }

  def getCondition(g: Grid, p: Position): Condition =
    g.getOrElse(p, Clean)

  def infectNode(g: Grid, p: Position): Grid = g.updated(p, Infected)

  def weakenNode(g: Grid, p: Position): Grid = g.updated(p, Weakened)

  def flagNode(g: Grid, p: Position): Grid = g.updated(p, Flagged)

  def cleanNode(g: Grid, p: Position): Grid = g - p

  def mkState(input: (Grid, Int)): State =
    input match { case ((g, size)) =>
      State(g, ((size/2), (size/2)), Up, 0)
    }

  def mkGrid(input: List[String]): (Grid, Int) =
    (input
      .zipWithIndex.map { case (l, r) => l.zipWithIndex.map { case (x, c) => ((r, c), x)}}
      .flatten
      .collect { case ((r, c), x) if x == '#' => ((r, c), Infected) }
      .toMap, input.size)

  def loadGrid(f: String): (Grid, Int) =
    mkGrid(io.Source.fromFile(f).getLines().filterNot(_.isEmpty).toList)

  val testData = """..#
                   |#..
                   |...""".stripMargin
}
