package advent

object Day11 {
  type Dir = Int
  val N  = 0
  val NE = 1
  val SE = 2
  val S  = 3
  val SW = 4
  val NW = 5

  // Combine two directions, wrapping around the hexagon
  def dirPlus(a: Dir, b: Int): Dir = (a + b) % 6

  case class PathState(steps: Map[Dir, Int]) {
    def stepCount: Int = steps.values.sum
    def stepCountDir(dir: Dir): Int = steps.getOrElse(dir, 0)
  }

  def day11(): Unit = {
    val input = loadData(inputFile)
    println(s"Day11.part1 = ${part1(input)}")
    println(s"Day11.part2 = ${part2(input)}")
  }

  def part1(input: List[Dir]): Int = {
    minimumPath(mkState(input)).stepCount
  }

  def part2(input: List[Dir]): Int = {
    furthest(input)
  }

  def minimumPath(state: PathState): PathState =
    reduceAll(state)

  def furthest(input: List[Dir]): Int = {
    def helper(dirs: Stream[List[Dir]], f: Int): Int = {
      dirs match {
        case Stream.Empty => f
        case h #:: t      => helper(t, math.max(minimumPath(mkState(h)).stepCount, f))
      }
    }

    helper(input.inits.toStream, 0)
  }

  /** Cancel steps on opposite hexagonal directions.
    *
    * For example, if the state contains 3 N steps and 2 S steps
    * the result should be a single N step.
    */
  def cancelOpposite(dir: Dir)(state: PathState): PathState = {
    val dir2 = dirPlus(dir, 3)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2)) match {
        case (_, 0)           => List()
        case (0, _)           => List()
        case (a, b) if a >= b  => List((dir -> (a-b)), (dir2 -> 0))
        case (a, b)           => List((dir -> 0),     (dir2 -> (b-a)))

      }
    PathState(state.steps ++ changes)
  }

  /** Cancel steps 'adjacent' steps.
    *
    * For example one N step and one SE step reduce to a single NE
    * step.
    */
  def reduceAdjacent(dir: Dir)(state: PathState): PathState = {
    val dir2 = dirPlus(dir, 2)
    val dir3 = dirPlus(dir, 1)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2), state.stepCountDir(dir3)) match {
        case (_, 0, _)           => List()
        case (0, _, _)           => List()
        case (a, b, c) if a >= b  => List((dir -> (a-b)), (dir2 -> 0)    , (dir3 -> (c+b)))
        case (a, b, c)           => List((dir -> 0)    , (dir2 -> (b-a)), (dir3 -> (c+a)))
      }
    PathState(state.steps ++ changes)
  }

  /** Apply all reductions to the given PathState
    */
  val reduceAll: PathState => PathState =
    (
      (N to NW).map(d => reduceAdjacent(d)(_)) ++   // Cancel all adjacent directions
      (N to SE).map(d => cancelOpposite(d)(_))      // Cancel all opposite directions
    ).reduce(_ compose _)                           // Compose everything into a single function



  def mkDir(s: String): Dir =
    s match {
      case "n"  => N
      case "ne" => NE
      case "se" => SE
      case "s"  => S
      case "sw" => SW
      case "nw" => NW
      case d    => throw new Exception(s"Bad direction $d")
    }

  def dir2Str(d: Dir): String =
    d match {
      case N  => "n"
      case NE => "ne"
      case SE => "se"
      case S  => "s"
      case SW => "sw"
      case NW => "nw"
      case d  => throw new Exception(s"Bad direction $d")
    }

  def mkState(steps: List[Dir]): PathState = {
    val ss = steps.groupBy(d => d).mapValues(_.size)
    PathState(ss)
  }

  val inputFile = "data/Day11.txt"

  def loadData(f: String): List[Dir] =
    io.Source.fromFile(f)
      .getLines
      .flatMap(_.split(",").map(mkDir))
      .toList


  def mkString(state: PathState): String = {
    val ss =
      state.steps
        .filter { case (k, v) => v > 0 }
        .toList
        .flatMap { case (k, v) => List.fill(v)(k) }
        .map(dir2Str)
        .mkString(",")

    s"${state.stepCount}:  $ss"
  }

}
