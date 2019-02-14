package advent

object Day25 {
  type StateName = String

  sealed trait Direction
  case object  Right extends Direction
  case object  Left  extends Direction

  case class TuringMachine(state: StateName, diagCount: Long, tape: Tape, states: States)

  case class Action(write: Int, move: Direction, next: StateName)

  case class State(name: StateName, zeroAction: Action, oneAction: Action)

  type States = Map[StateName, State]

  case class Tape(c: Long, tape: Set[Long])
  def emptyTape = Tape(0, Set.empty[Long])

  def day25(): Unit = {
    val machine = parseFile(inputFile)
    println(s"Day25.part1 = ${part1(machine)}")
  }

  def part1(machine: TuringMachine): Int = {
    val result = runMachine(machine)
    result.tape.tape.size
  }

  def runAction(a: Action, t: Tape): (Tape, StateName) =
    a match { case Action(w, m, n) =>
      (move(write(t, w), m), n)
    }

  def stepMachine(m: TuringMachine): TuringMachine =
    m match { case TuringMachine(curr, count, tape, states) =>
      if(count == 0) m
      else {
        val action =
          read(tape) match {
            case 0 => states(curr).zeroAction
            case 1 => states(curr).oneAction
          }
        val (t, n) = runAction(action, tape)
        TuringMachine(n, count-1, t, states)
      }
    }

  def runMachine(m: TuringMachine): TuringMachine = {
    if(m.diagCount == 0) m
    else                runMachine(stepMachine(m))
  }

  def write(t: Tape, v: Int): Tape =
    t match { case Tape(c, tape) =>
      v match {
        case 0 => t.copy(tape= tape-c)
        case 1 => t.copy(tape= tape+c)
      }
    }

  def read(t: Tape): Int = if(t.tape.contains(t.c)) 1 else 0

  def move(t: Tape, d: Direction): Tape =
    d match {
      case Right => t.copy(c=t.c+1)
      case Left  => t.copy(c=t.c-1)
    }

  def checksum(t: Tape): Int = t.tape.size

  def checksum(m: TuringMachine): Int = checksum(m.tape)

  val initialStateRegex = """Begin in state (\w+).""".r
  val diagRegex = """Perform a diagnostic checksum after (\d+) steps.""".r

  def parseIntro(lines: Vector[String]): (StateName, Long) = {
    val initialStateRegex(name) = lines(0)
    val diagRegex(count) = lines(1)
    (name, count.toLong)
  }

  val stateNameRegex = """In state (\w+):""".r
  val zeroValueRegex = """If the current value is 0:""".r
  val oneValueRegex  = """If the current value is 0:""".r
  val writeRegex     = """- Write the value (\d).""".r
  val moveRegex      = """- Move one slot to the (\w+).""".r
  val nextStateRegex = """- Continue with state (\w+).""".r

  def parseDirection(s: String): Direction =
    s match {
      case "right" => Right
      case "left"  => Left
      case _       => throw new Exception(s"Invalid direction $s")
    }

  def parseState(lines: Vector[String]): State = {
    val stateNameRegex(n)  = lines(0).trim
    val writeRegex(v0)     = lines(2).trim
    val moveRegex(d0)      = lines(3).trim
    val nextStateRegex(s0) = lines(4).trim
    val writeRegex(v1)     = lines(6).trim
    val moveRegex(d1)      = lines(7).trim
    val nextStateRegex(s1) = lines(8).trim

    val zeroAction = Action(v0.toInt, parseDirection(d0), s0)
    val oneAction =  Action(v1.toInt, parseDirection(d1), s1)

    State(n, zeroAction, oneAction)
  }

  def splitOnBlankLines(lines: Iterator[String]): Vector[Vector[String]] = {
    lines.foldLeft(Vector(Vector.empty[String])){ (a, l) =>
      if(l.isEmpty) a :+ Vector.empty[String]
      else          a.updated(a.size-1, a.last :+ l)
    }
  }

  def parseFile(f: String): TuringMachine = {
    val segs = splitOnBlankLines(io.Source.fromFile(f).getLines())

    val (n, c) = parseIntro(segs.head)
    val states = segs.tail.map(parseState).map(s => (s.name, s)).toMap

    TuringMachine(n, c, emptyTape, states)
  }

  val inputFile = "data/Day25.txt"
}
