package org.saegesser

import scala.io._

object Day16 {

  type Dancers = Vector[Char]
  type Dance = List[Move]

  sealed trait Move
  case class Spin(x: Int)                extends Move
  case class Exchange(x: Int, y: Int)      extends Move
  case class Partner(a: Char, b: Char) extends Move

  def move(dancers: Dancers, move: Move): Dancers =
    move match {
      case Spin(x)        =>
        val (f, b) = dancers.splitAt(dancers.size-x)
        b ++ f
      case Exchange(x, y) =>
        val a = dancers(x)
        dancers.updated(x, dancers(y)).updated(y, a)
      case Partner(a, b)  =>
        val x = dancers.indexOf(a)
        val y = dancers.indexOf(b)
        val c = dancers(x)
        dancers.updated(x, dancers(y)).updated(y, c)
    }

  def dance(dancers: Dancers, dance: Dance): Dancers = {
    dance.foldLeft(dancers)(move)
  }

  def danceAgain(dancers: Dancers, d: Dance, count: Long): Dancers = {
    def helper(remaining: Long, ds: Dancers): Dancers =
      if(remaining == 0) ds
      else              helper(remaining-1, dance(ds, d))

    helper(count, dancers)
  }

  def findCycle(dancers: Dancers, d: Dance): Int = {
    def helper(accum: List[String], ds: Dancers): Int = {
      val dd = dance(ds, d)
      if(accum.contains(dd.mkString)) accum.size
      else                            helper(dd.mkString :: accum, dd)
    }

    helper(List(), dancers)
  }


  def processFile(dancers: Dancers, f: String): Dancers = {
    case class State(dancers: Dancers, buf: String)
    Source.fromFile(f)
      .foldLeft(State(dancers, "")) { case s@(State(d,  b), c) =>
        println(s"$s")
        if(c == ',')             State(move(d, parseMove(b.trim)), "")
        else if(c.isWhitespace) s._1
        else                    State(d, b + c)
      }
    .dancers
  }

  def parseFile(f: String): Dance =
    Source.fromFile(f).mkString.split(",").map(s => parseMove(s.trim)).toList

  def parseSpin(s: String): Spin =
    Spin(s.drop(1).toInt)

  def parseExchange(s: String): Exchange = {
    val fields = s.drop(1).split("/")
    Exchange(fields(0).toInt, fields(1).toInt)
  }

  def parsePartner(s: String): Partner = {
    val fields = s.drop(1).split("/")
    Partner(fields(0).head, fields(1).head)
  }

  def parseMove(s: String): Move =
    s.head match {
      case 's' => parseSpin(s)
      case 'x' => parseExchange(s)
      case 'p' => parsePartner(s)
      case _   => throw new Exception(s"Bad input $s")
    }

  val initialDancers = "abcdefghijklmnop".toVector
}
