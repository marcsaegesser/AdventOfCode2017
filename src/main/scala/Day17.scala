package org.saegesser

import scala.math.Integral.Implicits._

object Day17 {

  case class BufferState(buf: Vector[Int], current: Int) {
    def insertAt(i: Int, v: Int): BufferState = {
      val (a, b) = buf.splitAt(i)
      BufferState(a ++ Vector(v) ++ b, i)
    }

    def nextPosition(step: Int): Int =
      ((step/%buf.size)._2 + current)%buf.size + 1
  }

  def nextIndex(current: Int, v: Int, step: Int): Int =
    ((step/%v)._2 + current)%v + 1

  def oneCycle(state: BufferState, v: Int, step: Int): BufferState = {
    state.insertAt(state.nextPosition(step), v)
  }

  def runBuffer2(cycles: Int, step: Int): Int = {
    def helper(accum: Int, current: Int, v: Int): Int =
      if(v > cycles) accum
      else {
        val next = nextIndex(current, v, step)
        if(next == 1) helper(v,     next, v+1)
        else         helper(accum, next, v+1)
      }

    helper(0, 0, 1)
  }

  def runBuffer(cycles: Int, step: Int): BufferState = {
    def helper(state: BufferState, v: Int): BufferState =
      if(v > cycles) state
      else helper(state.insertAt(state.nextPosition(step), v), v+1)

    helper(BufferState(Vector(0), 0), 1)
  }

  val puzzleInput = 380
}
