package advent

import scala.io._

object Day19 {
  type Maze = Vector[Vector[Char]]

  val VertPipe = '|'
  val HorizPipe = '-'
  val Turn = '+'
  val Empty = ' '


  sealed trait Direction
  case object N extends Direction
  case object E extends Direction
  case object S extends Direction
  case object W extends Direction

  case class State(r: Int, c: Int, dir: Direction, l: List[Char], steps: Int)

  def stepNorth(state: State): State = State(state.r-1, state.c, N, state.l, state.steps+1)
  def stepEast(state: State): State  = State(state.r, state.c+1, E, state.l, state.steps+1)
  def stepSouth(state: State): State = State(state.r+1, state.c, S, state.l, state.steps+1)
  def stepWest(state: State): State  = State(state.r, state.c-1, W, state.l, state.steps+1)


  def takeTurn(maze: Maze, state: State): State= {
    state match { case State(r, c, d, l, s) =>
      d match {
        case N => if(maze(r)(c-1) != Empty) stepWest(state)  else stepEast(state)
        case E => if(maze(r-1)(c) != Empty) stepNorth(state) else stepSouth(state)
        case S => if(maze(r)(c-1) != Empty) stepWest(state)  else stepEast(state)
        case W => if(maze(r-1)(c) != Empty) stepNorth(state) else stepSouth(state)
      }
    }
  }

  def traverse(maze: Maze): State = {
    def helper(state: State): State = {
      state match { case State(r, c, d, l, s) =>
        d match {
          case N =>
            maze(r)(c) match {
              case Empty     => state
              case VertPipe  => helper(stepNorth(state))
              case HorizPipe => helper(stepNorth(state))
              case Turn      => helper(takeTurn(maze, state))
              case n         => helper(stepNorth(state.copy(l=n :: l)))
            }
          case E =>
            maze(r)(c) match {
              case Empty     => state
              case VertPipe  => helper(stepEast(state))
              case HorizPipe => helper(stepEast(state))
              case Turn      => helper(takeTurn(maze, state))
              case n         => helper(stepEast(state.copy(l=n :: l)))
            }
          case S =>
            maze(r)(c) match {
              case Empty     => state
              case VertPipe  => helper(stepSouth(state))
              case HorizPipe => helper(stepSouth(state))
              case Turn      => helper(takeTurn(maze, state))
              case n         => helper(stepSouth(state.copy(l=n :: l)))
            }
          case W =>
            maze(r)(c) match {
              case Empty     => state
              case VertPipe  => helper(stepWest(state))
              case HorizPipe => helper(stepWest(state))
              case Turn      => helper(takeTurn(maze, state))
              case n         => helper(stepWest(state.copy(l=n :: l)))
            }
        }
      }
    }

    helper(mkInitialState(maze))
  }

  def mkInitialState(maze: Maze): State =
    State(1, findStartColumn(maze), S, List.empty[Char], 0)

  def findStartColumn(maze: Maze): Int =
    maze(1).indexOf('|')

  def mkRow(row: String): Vector[Char] =
    Vector(' ') ++ row.toVector ++ Vector(' ')

  def readMaze(f: String): Maze = {
    val rows = Source.fromFile(f).getLines().filterNot(_.isEmpty).map(_.toVector).toVector
    val maxLength = rows.map(_.size).max
    val blank = Vector(Vector.fill(maxLength)(' '))

    blank ++ rows ++ blank
  }
}
