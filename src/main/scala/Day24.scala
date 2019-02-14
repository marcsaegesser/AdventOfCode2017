package advent

import scala.annotation._

object Day24 {
  case class Component(id: Int, p0: Int, p1: Int) {
    val strength: Int = p0 + p1
    def hasPort(pinCount: Int): Boolean = p0 == pinCount || p1 == pinCount
    override def toString = s"$p0/$p1"
  }
  case class Bridge(cs: List[Component], availPins: Int) {
    lazy val length   = cs.length
    lazy val strength = cs.map(_.strength).sum
  }

  implicit val bridgeOrdering = Ordering[(Int, Int)].on((b: Bridge) => (b.length, b.strength)).reverse

  case class State(bridge: Bridge, available: List[Component])

  def mkState(cs: List[Component]): State =
    State(initialBridge, cs)

  def day24(): Unit = {
    val components = parseFile(inputFile)
    println(s"Day24.part1 = ${part1(components)}")
    println(s"Day24.part2 = ${part2(components)}")
  }

  def part1(components: List[Component]): Int = {
    evalStrength(mkState(components))
  }

  def part2(components: List[Component]): Int = {
    val b = evalLengthStrength(mkState(components))
    bridgeStrength(b)
  }

  def partitionFits(cs: List[Component], pinCount: Int): (List[Component], List[Component]) =
    cs.partition(c => c.hasPort(pinCount))

  def addBridgeComponent(b: Bridge, c: Component): Bridge =
    if(b.availPins == c.p0) Bridge(c :: b.cs, c.p1)
    else                   Bridge(c :: b.cs, c.p0)

  def bridgeStrength(b: Bridge): Int = b.cs.map(_.strength).sum

  def bridgesFrom(state: State): List[State] = {
    state match { case State(b, a) =>
      val (fits, rest) = partitionFits(a, b.availPins)
      fits.map(c => State(addBridgeComponent(b, c), a.filterNot(_.id == c.id)))
    }
  }

  def evalStrength(state: State): Int =
    state match { case State(b, a) =>
      val next = bridgesFrom(state)
      if(next.isEmpty) bridgeStrength(b)
      else             next.map(evalStrength).max
    }

  def evalLengthStrength(state: State): Bridge =
    state match { case State(b, a) =>
      val next = bridgesFrom(state)
      if(next.isEmpty) b
      else             next.map(evalLengthStrength).sorted.head
    }

  val initialBridge = Bridge(List.empty[Component], 0)

  val componentRegex = """(\d+)/(\d+)""".r
  def parseComponent(id: Int, s: String): Component = {
    val componentRegex(p0, p1) = s.trim
    Component(id, p0.toInt, p1.toInt)
  }

  def parseFile(f: String): List[Component] = {
    io.Source.fromFile(f)
      .getLines()
      .filterNot(_.isEmpty)
      .zipWithIndex
      .map{ case (l, i) => parseComponent(i, l) }
      .toList
  }

  val inputFile = "data/Day24.txt"
}
