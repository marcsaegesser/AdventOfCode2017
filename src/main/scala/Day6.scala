package org.saegesser

object Day6 {
  def chooseBiggest(v: Vector[Int]): Int =
    v.zipWithIndex
      .foldLeft((0, 0)) { case ((m, mi), (a, i)) => if(a > m) (a, i ) else (m, mi) }
      ._2

  def redistribute(v: Vector[Int]): Vector[Int] = {
    def helper(accum: Vector[Int], i: Int, remaining: Int): Vector[Int] =
      if(remaining == 0) accum
      else helper(accum.updated(i, accum(i)+1), (i+1)%v.size, remaining-1)

    val at = chooseBiggest(v)
    val toDist = v(at)
    val initial = v.updated(at, 0)
    helper(initial, (at+1)%v.size, toDist)
  }

  def untilCycle(input: Vector[Int]): Int = {
    def runCycle(accum: List[Vector[Int]], v: Vector[Int]): List[Vector[Int]] =
      if(accum.contains(v)) accum
      else                  runCycle(v :: accum, redistribute(v))

    runCycle(List.empty[Vector[Int]], input).size
  }

  def cycleLength(input: Vector[Int]): Int = {
    def runCycle(accum: List[Vector[Int]], v: Vector[Int]): (List[Vector[Int]], Vector[Int]) =
      if(accum.contains(v)) (accum, v)
      else                  runCycle(v :: accum, redistribute(v))

    val repeated = runCycle(List.empty[Vector[Int]], input)
    val cycle = runCycle(List.empty[Vector[Int]], repeated._2)
    cycle._1.size
  }

  def mkVector(s: String): Vector[Int] = s.split("\\s+").map(_.toInt).toVector

  val puzzleData = "11	11	13	7	0	15	5	5	4	4	1	1	7	1	15	11"
}
