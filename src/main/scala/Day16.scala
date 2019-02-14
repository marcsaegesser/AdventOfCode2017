package advent

object Day16 {

  type Dancers = Vector[Char]
  type Dance = List[Move]

  def day16(): Unit = {
    val dance = loadFile(inputFile)
    println(s"Day16.part1 = ${part1(initialDancers, dance)}")
    println(s"Day16.part2 = ${part2(initialDancers, dance)}")
  }

  def part1(dancers: Dancers, d: Dance): String =
    dance(dancers, d).mkString

  def part2(dancers: Dancers, d: Dance): String = {
    val cycleLength = findCycle(dancers, d)
    val remainingDances = 1000000000 % cycleLength
    (1 to remainingDances).foldLeft(dancers) { case (ds, _) => dance(ds, d) }.mkString
  }

  sealed trait Move
  case class Spin(x: Int)                extends Move
  case class Exchange(x: Int, y: Int)      extends Move
  case class Partner(a: Char, b: Char) extends Move

  def move(dancers: Dancers, move: Move): Dancers = {
    def swap(ds: Dancers, x: Int, y: Int): Dancers = {
        val c = dancers(x)
        dancers.updated(x, dancers(y)).updated(y, c)
    }

    move match {
      case Spin(x)        =>
        val (f, b) = dancers.splitAt(dancers.size-x)
        b ++ f
      case Exchange(x, y) => swap(dancers, x, y)
      case Partner(a, b)  => swap(dancers, dancers.indexOf(a), dancers.indexOf(b))
    }
  }

  def dance(dancers: Dancers, dance: Dance): Dancers = {
    dance.foldLeft(dancers)(move)
  }

  def findCycle(dancers: Dancers, d: Dance): Int = {
    def helper(accum: List[String], ds: Dancers): Int = {
      val dd = dance(ds, d)
      if(accum.contains(dd.mkString)) accum.size
      else                            helper(dd.mkString :: accum, dd)
    }

    helper(List(), dancers)
  }


  val parseSpin     = """s(\d+)""".r
  val parseExchange = """x(\d+)/(\d+)""".r
  val parsePartner  = """p(\w)/(\w)""".r

  def parseMove(s: String): Move =
    s match {
      case parseSpin(x)        => Spin(x.toInt)
      case parseExchange(a, b) => Exchange(a.toInt, b.toInt)
      case parsePartner(a, b)  => Partner(a.head, b.head)
      case _                   => throw new Exception(s"Invalid move '$s'")
    }

  def loadFile(f: String): Dance =
    io.Source.fromFile(f)
      .getLines().take(1).mkString
      .split(",")
      .map(parseMove)
      .toList

  val inputFile = "data/Day16.txt"
  val initialDancers = "abcdefghijklmnop".toVector
}
