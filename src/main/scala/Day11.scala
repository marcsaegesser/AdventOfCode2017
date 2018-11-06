package org.saegesser

import scala.io._

object Day11 {
  type Dir = Int
  val N  = 0
  val NE = 1
  val SE = 2
  val S  = 3
  val SW = 4
  val NW = 5

  def dirPlus(a: Dir, b: Int): Dir = (a + b) % 6

  case class PathState(steps: Map[Dir, Int], stepCount: Int) {
    def stepCountDir(dir: Dir): Int = steps.getOrElse(dir, 0)
  }

  def cancelOpposite(state: PathState, dir: Dir): PathState = {
    val dir2 = dirPlus(dir, 3)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2)) match {
        case (a, 0)          => (List(), 0)
        case (0, b)          => (List(), 0)
        case (a, b) if a == b => (List((dir -> 0),     (dir2 -> 0))    , -2*a)
        case (a, b) if a < b => (List((dir -> 0),     (dir2 -> (b-a))), -2*a)
        case (a, b)          => (List((dir -> (a-b)), (dir2 -> 0))    , -2*b)
      }
    PathState(state.steps ++ changes._1, state.stepCount + changes._2)
  }

  def reduceAdjacent(state: PathState, dir: Dir): PathState = {
    val dir2 = dirPlus(dir, 2)
    val dir3 = dirPlus(dir, 1)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2), state.stepCountDir(dir3)) match {
        case (a, 0, _)          => (List(), 0)
        case (0, b, _)          => (List(), 0)
        case (a, b, c) if a == b => (List((dir -> 0)    , (dir2 -> 0)    , (dir3 -> (c+a))), -a)
        case (a, b, c) if a < b => (List((dir -> 0)    , (dir2 -> (b-a)), (dir3 -> (c+a))), -a)
        case (a, b, c)          => (List((dir -> (a-b)), (dir2 -> 0)    , (dir3 -> (c+b))), -b)
      }
    PathState(state.steps ++ changes._1, state.stepCount + changes._2)
  }

  val reduceAll =
    ((0 to 2).map(d => cancelOpposite(_: PathState, d)) ++ (0 to 5).map(d => reduceAdjacent(_: PathState, d))).reduce(_ compose _)

  def minimumPath(state: PathState): PathState = {
    def helper(state: PathState, prevSize: Int): PathState = {
      val s = reduceAll(state)
      if(s.stepCount == state.stepCount) s
      else helper(s, s.stepCount)
    }

    helper(state, state.stepCount)
  }

  def furthest(input: List[Dir]): Int = {
    def helper(dirs: Stream[List[Dir]], f: Int): Int = {
      dirs match {
        case Stream.Empty => f
        case h #:: t      => helper(t, math.max(minimumPath(mkState(h)).stepCount, f))
      }
    }

    helper(input.inits.toStream, 0)
  }

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
    PathState(ss, ss.values.sum)
  }

  def parseInput(input: String): PathState = {
    val steps = input.split(",").map(s => mkDir(s.trim)).groupBy(d => d).mapValues(_.size)
    PathState(steps, steps.values.sum)
  }

  def parseDirs(input: String): List[Dir] =
    input.split(",").map(s => mkDir(s.trim)).toList

  def loadFile(f: String): String = {
    Source.fromFile(f).toList.mkString
  }


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
