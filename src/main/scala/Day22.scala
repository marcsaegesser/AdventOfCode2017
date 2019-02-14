package advent

/** An implementation of Day22 part 1
  */
object Day22a {
  case class Position(r: Int, c: Int) {
    def right = Position(r,   c+1)
    def up    = Position(r-1, c)
    def left  = Position(r,   c-1)
    def down  = Position(r+1, c)
  }

  type Grid = Set[Position]

  sealed trait Condition
  case object Clean    extends Condition
  case object Infected extends Condition

  def part1(grid: Grid): Int = {
    iterateN(mkState(grid), 10000).count
  }

  type Direction = Int
  val Right: Direction = 0
  val Up: Direction    = 1
  val Left: Direction  = 2
  val Down: Direction  = 3
  val NumDirections: Int = 4

  case class State(g: Grid, p: Position, d: Direction, count: Int)

  def burst(state: State): State =
    state match { case State(g, p, d, count) =>
      getCondition(g, p) match {
        case Clean    =>
          val newD = turnLeft(d)
          State(infectNode(g, p), move(p, newD), newD, count+1)
        case Infected =>
          val newD = turnRight(d)
          State(cleanNode(g, p), move(p, newD), newD, count)
      }
    }

  def iterateN(state: State, n: Int): State =
    if(n == 0) state
    else      iterateN(burst(state), n-1)

  def turnLeft(d: Direction): Direction = (d+1)%NumDirections

  def turnRight(d: Direction): Direction = (d-1+NumDirections)%NumDirections

  def move(p: Position, d: Direction): Position =
    d match {
      case Right => p.right
      case Up    => p.up
      case Left  => p.left
      case Down  => p.down
    }

  def getCondition(g: Grid, p: Position): Condition =
    if(g.contains(p)) Infected
    else              Clean

  def infectNode(g: Grid, p: Position): Grid = g + p

  def cleanNode(g: Grid, p: Position): Grid = g - p

  def mkState(grid: Grid): State =
    State(grid, Position(0, 0), Up, 0)

  // NOTE: The input is *always* a square with an odd size.
  def mkGrid(input: List[String]): Grid = {
    val size = input.size/2
    def indices = Stream.from(-size)

    input.zip(indices)
      .flatMap { case (l, r) => l.zip(indices).map { case (x, c) => (Position(r, c), x) } }
      .collect { case (Position(r, c), x) if x == '#' => Position(r, c) }
      .toSet
  }

  def loadGrid(f: String): Grid =
    mkGrid(io.Source.fromFile(f).getLines().filterNot(_.isEmpty).toList)

  val inputFile = "data/Day22.txt"
}

/** The full Day 22 implementation
  */
object Day22 {
  case class Position(r: Int, c: Int) {
    def right = Position(r,   c+1)
    def up    = Position(r-1, c)
    def left  = Position(r,   c-1)
    def down  = Position(r+1, c)
  }

  type Grid = Map[Position, Condition]

  def day22(): Unit = {
    val grid = loadGrid(inputFile)
    println(s"Day22.part1 = ${part1(grid)}")
    println(s"Day22.part2 = ${part2(grid)}")
  }

  def part1(grid: Grid): Int =
    iterateN(burst1, mkState(grid), 10000).count

  def part2(grid: Grid): Int =
    iterateN(burst2, mkState(grid), 10000000).count

  sealed trait Condition
  case object Clean    extends Condition
  case object Weakened extends Condition
  case object Infected extends Condition
  case object Flagged  extends Condition


  type Direction = Int
  val Right: Direction = 0
  val Up: Direction    = 1
  val Left: Direction  = 2
  val Down: Direction  = 3
  val NumDirections: Int = 4

  case class State(g: Grid, p: Position, d: Direction, count: Int)

  def burst1(state: State): State =
    state match { case State(g, p, d, count) =>
      getCondition(g, p) match {
        case Clean    =>
          val newD = turnLeft(d)
          State(infectNode(g, p), move(p, newD), newD, count+1)
        case Infected =>
          val newD = turnRight(d)
          State(cleanNode(g, p), move(p, newD), newD, count)
        case Weakened => ???
        case Flagged  => ???
      }
    }

  def burst2(state: State): State =
    state match { case State(g, p, d, count) =>
      getCondition(g, p) match {
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

  def iterateN(b: State => State, state: State, n: Int): State =
    if(n == 0) state
    else      iterateN(b, b(state), n-1)

  def turnLeft(d: Direction): Direction = (d+1)%NumDirections

  def turnRight(d: Direction): Direction = (d-1+NumDirections)%NumDirections

  def reverse(d: Direction): Direction = (d+NumDirections/2)%NumDirections

  def move(p: Position, d: Direction): Position =
    d match {
      case Right => p.right
      case Up    => p.up
      case Left  => p.left
      case Down  => p.down
    }

  def getCondition(g: Grid, p: Position): Condition =
    g.getOrElse(p, Clean)

  def infectNode(g: Grid, p: Position): Grid = g.updated(p, Infected)

  def weakenNode(g: Grid, p: Position): Grid = g.updated(p, Weakened)

  def flagNode(g: Grid, p: Position): Grid = g.updated(p, Flagged)

  def cleanNode(g: Grid, p: Position): Grid = g - p

  def mkState(grid: Grid): State =
    State(grid, Position(0, 0), Up, 0)

  // NOTE: The input is *always* a square with an odd size.
  def mkGrid(input: List[String]): Grid = {
    val size = input.size/2
    def indices = Stream.from(-size)

    input.zip(indices)
      .flatMap { case (l, r) =>                 // A line and its row index
        l.zip(indices)
          .collect { case (x, c) if x == '#' => // A character and its column index
            (Position(r, c), Infected)          // An infected location and its position
          }
      }
      .toMap
  }

  def loadGrid(f: String): Grid =
    mkGrid(io.Source.fromFile(f).getLines().filterNot(_.isEmpty).toList)

  val inputFile = "data/Day22.txt"
}
