package org.saegesser

import scala.math.Integral.Implicits._

object Day10 {
  case class HashState(data: Vector[Int], position: Int, skipSize: Int)

  val initialState = HashState((0 to 255).toVector, 0, 0)

  def rotate[A](v: Vector[A], l: Int): Vector[A] = {
    val (a, b) = v.splitAt(v.size - l)
    b ++ a
  }

  def runCycle(state: HashState, length: Int): HashState = {

    state match {
      case HashState(d, p, s) =>
        (p + length) /% d.size match {
          case (0, _) => // No wrap
            val (a, b, c) = (d.slice(0, p), d.slice(p, p+length), d.slice(p+length, d.size))
            HashState(a ++ b.reverse ++ c, (p + length + s)%d.size, s+1)
          case (1, r) => // Wrap
            val (a, b, c) = (d.slice(0, r), d.slice(r, p), d.slice(p, d.size))
            HashState(rotate(a.reverse ++ c.reverse ++ b, p) , (p + length + s)%d.size, s+1)
          case (_, _) => throw new Exception(s"Unexpected length $length. state=$state")
        }
    }
  }

  def generateHash(input: String): String =
    generateHash(mkInput(input))

  def generateHash(input: List[Int]): String = {
    def runRound(state: HashState, input: List[Int]): HashState =
      input.foldLeft(state)(runCycle)

    val sparse = (1 to 64).toList.foldLeft(initialState){ (s, _) => runRound(s, input) }
    sparse.data
      .sliding(16, 16)
      .map(_.reduce(_^_))
      .map(i => f"$i%02x")
      .mkString
  }

  def mkInput(in: String): List[Int] =
    in.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)

  val puzzleData = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
}
