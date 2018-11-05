package org.saegesser

import scala.io._

object Day9 {
  case class ParserState(depth: Int, inGarbage: Boolean, inCancel: Boolean, score: Int, count: Int)

  val initialState = ParserState(0, false, false, 0, 0)

  val begGrp: Char = 0x7b
  val endGrp: Char = 0x7d
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

  def parseFile(f: String): Iterator[ParserState] = {
    val source = Source.fromFile(f)
    source.getLines().map(parseStream)
  }
}
