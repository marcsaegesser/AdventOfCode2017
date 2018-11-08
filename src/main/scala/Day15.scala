package org.saegesser

import scala.math.Integral.Implicits._

object Day15 {

  case class GeneratorState(value: Long, factor: Long) {
    def next: GeneratorState = this.copy(value=((value * factor) /% divisor)._2)
    override def equals(that: Any): Boolean =
      that match {
        case that: GeneratorState => (this.value & 0x000000000000FFFF) == (that.value & 0x000000000000FFFF)
        case _                    => false
      }

    override def hashCode: Int = value.toInt
  }

  val judge = (a: Long, b: Long) => (a & 0x000000000000FFFF) == (b & 0x000000000000FFFF)

  def genStream(initial: GeneratorState): Stream[GeneratorState] = {
    def loop(s: GeneratorState): Stream[GeneratorState] = s.next #:: loop(s.next)
    loop(initial)
  }

  def genIter(seed: Long, factor: Long): Iterator[Long] = new Iterator[Long]{
    var curr = GeneratorState(seed, factor)
    def hasNext = true
    def next(): Long = {
      curr = curr.next
      curr.value
    }
  }

  def collectSample(genA: Iterator[Long], genB: Iterator[Long]): Int =
    (genA zip genB).take(40000000).filter { judge.tupled }.size


  def collectSample2(genA: Iterator[Long], genB: Iterator[Long]): Int = {
    val as = genA.filter(_%4 == 0)
    val bs = genB.filter(_%8 == 0)
    (as zip bs).take(5000000).filter(judge.tupled).size
  }

  val divisor = 2147483647L
  val generatorAStart = 722
  val generatorAFactor = 16807
  val generatorBStart = 354
  val generatorBFactor = 48271
}
