package org.saegesser

import scala.io._

object Day13 {
  type Severity = Int

  case class Layer(num: Int, depth: Int) {
    val limit: Int = (depth-1)*2
  }
  case class FirewallState(layers: List[Layer], time: Int, severity: Int)

  def traverseFirewall(layers: List[Layer]): Severity = {
    layers.foldLeft(0) { case (s, l) =>
      if(((l.num) % l.limit) == 0) s + l.num*l.depth
      else                                s
    }
  }

  def attemptFirewall(layers: List[Layer], delay: Int): Boolean = {
    layers.foldLeft(true) { case (c, l) =>
      c && (((l.num+delay) % l.limit) != 0)
    }
  }

  def determineDelay(layers: List[Layer]): Int = {
    def helper(delay: Int): Int = {
      if(attemptFirewall(layers, delay)) delay
      else                               helper(delay+1)
    }

    helper(0)
  }

  def parseLayer(s: String): Layer ={
    val fields = s.split(""":\s+""").map(_.toInt)
    Layer(fields(0), fields(1))
  }

  def fromString(s: String): List[Layer] =
    s.lines.map(l => parseLayer(l.trim)).toList

  def loadFile(f: String): List[Layer] =
    Source.fromFile(f).getLines.map(l => parseLayer(l.trim)).toList

  val sampleData = """0: 3
1: 2
4: 4
6: 4"""
}
