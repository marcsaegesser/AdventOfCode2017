package org.saegesser

object Day14 {
  case class Segment(used: Boolean, region: Int = 0) {
    override
    def toString: String =
      if(region > 0) s"${region%10}"
      else if(used) "#"
      else "."
  }
  type Matrix = Vector[Vector[Segment]]


  def mkMatrix(key: String): Matrix = {
    Vector.tabulate(128)(r => mkRowSegments(mkRow(key, r)))
  }

  def mkRow(key: String, r: Int): String = Day10.generateHash(s"$key-$r")

  def mkRowSegments(row: String): Vector[Segment] = {
    row.toVector.map(nibleSegments).flatten
  }

  def computeUsage(key: String): Int = {
    def countStringBits(s: String): Int = s.toSeq.map(nibleCount).sum

    (0 to 127).foldLeft(0) { case (s, r) => s + countStringBits(mkRow(key, r)) }
  }

  def adjacencies(r: Int, c: Int): List[(Int, Int)] = {
    List((r-1, c), (r, c+1), (r+1, c), (r, c-1)).collect { case (a, b) if a>=0 && a<128 && b>=0 && b<128 => (a, b) }
  }

  def updateMatrix(m: Matrix, r: Int, c: Int, region: Int): Matrix =
    m.updated(r, m(r).updated(c, m(r)(c).copy(region=region)))

  def computeRegion(toCheck: List[(Int, Int)], matrix: Matrix, region: Int, changed: Boolean = false): (Matrix, Int) =
    toCheck match {
      case Nil          => (matrix, if(changed) region+1 else region)
      case (r, c) :: t  =>
        val seg = matrix(r)(c)
        if(seg.used && seg.region == 0) computeRegion(adjacencies(r, c) ++ t, updateMatrix(matrix, r, c, region), region, true)
        else computeRegion(t, matrix, region, changed)

    }

  def computeRegions(matrix: Matrix): (Matrix, Int) = {
    val coords =
      for {
        r <- 0 to 127
        c <- 0 to 127
      } yield (r, c)

    coords.foldLeft((matrix, 1)) { case ((m, r), c) => computeRegion(List(c), m, r) }
  }

  def showMatrix(matrix: Matrix): String = {
    def showRow(r: Vector[Segment]): String = r.map(_.toString).mkString

    matrix.map(showRow).mkString("\n")
  }


  val nibleSegments = Map[Char, Vector[Segment]](
      '0' -> Vector(Segment(false), Segment(false), Segment(false), Segment(false))
    , '1' -> Vector(Segment(false), Segment(false), Segment(false), Segment(true))
    , '2' -> Vector(Segment(false), Segment(false), Segment(true), Segment(false))
    , '3' -> Vector(Segment(false), Segment(false), Segment(true), Segment(true))
    , '4' -> Vector(Segment(false), Segment(true), Segment(false), Segment(false))
    , '5' -> Vector(Segment(false), Segment(true), Segment(false), Segment(true))
    , '6' -> Vector(Segment(false), Segment(true), Segment(true), Segment(false))
    , '7' -> Vector(Segment(false), Segment(true), Segment(true), Segment(true))
    , '8' -> Vector(Segment(true), Segment(false), Segment(false), Segment(false))
    , '9' -> Vector(Segment(true), Segment(false), Segment(false), Segment(true))
    , 'a' -> Vector(Segment(true), Segment(false), Segment(true), Segment(false))
    , 'b' -> Vector(Segment(true), Segment(false), Segment(true), Segment(true))
    , 'c' -> Vector(Segment(true), Segment(true), Segment(false), Segment(false))
    , 'd' -> Vector(Segment(true), Segment(true), Segment(false), Segment(true))
    , 'e' -> Vector(Segment(true), Segment(true), Segment(true), Segment(false))
    , 'f' -> Vector(Segment(true), Segment(true), Segment(true), Segment(true))
  )


  val nibleCount = Map[Char, Int](
      '0' -> countBits(0x00)
    , '1' -> countBits(0x01)
    , '2' -> countBits(0x02)
    , '3' -> countBits(0x03)
    , '4' -> countBits(0x04)
    , '5' -> countBits(0x05)
    , '6' -> countBits(0x06)
    , '7' -> countBits(0x07)
    , '8' -> countBits(0x08)
    , '9' -> countBits(0x09)
    , 'a' -> countBits(0x0a)
    , 'b' -> countBits(0x0b)
    , 'c' -> countBits(0x0c)
    , 'd' -> countBits(0x0d)
    , 'e' -> countBits(0x0e)
    , 'f' -> countBits(0x0f)
  )

  val bitCountTable = (0 to 255).toVector.map(countBits)

  def countBits(b: Int): Int =
    (0 to 7).foldLeft(0) { case (s, c) => s + (b>>c & 0x01) }
}
