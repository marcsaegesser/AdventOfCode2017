package advent

object Day09 {
  case class ParserState(depth: Int, inGarbage: Boolean, inCancel: Boolean, score: Int, count: Int)

  val initialState = ParserState(0, false, false, 0, 0)

  def day09(): Unit = {
    val s = loadFile(inputFile)
    println(s"Day09.part1 = ${part1(s)}")
    println(s"Day09.part2 = ${part2(s)}")
  }

  def part1(input: String): Int = {
    parseStream(input).score
  }

  def part2(input: String): Int = {
    parseStream(input).count
  }

  val begGrp = 0x7b.toChar
  val endGrp = 0x7d.toChar
  val begGbg = '<'
  val endGbg = '>'
  val cancel = '!'

  def parseChar(c: Char, state: ParserState): ParserState = {
    state match {
      case ParserState(_, true, true, _, _)               => state.copy(inCancel=false)   // Cancel the current char, leave cancel
      case ParserState(_, true, false, _, _) if c == cancel => state.copy(inCancel=true)
      case ParserState(_, true, false, _, _) if c == endGbg => state.copy(inGarbage=false)  // End of garbage
      case ParserState(_, true, false, _, t)               => state.copy(count=state.count+1)
      case ParserState(d, false, false, s, _) if c == begGbg => state.copy(inGarbage=true)
      case ParserState(d, false, false, s, t) if c == begGrp => state.copy(depth=state.depth+1, score=state.score+state.depth+1)
      case ParserState(d, false, false, s, t) if c == endGrp => state.copy(depth=state.depth-1)
      case ParserState(_, _, _, _, t)               => state
    }
  }

  def parseStream(stream: String): ParserState = {
    stream.foldLeft(initialState){ case (s, c) => parseChar(c, s) }
  }

  def loadFile(f: String): String = {
    io.Source.fromFile(f).getLines().mkString
  }

  val inputFile = "data/Day9.txt"
}
