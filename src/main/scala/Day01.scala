package advent

object Day01_A {
  val inputFile = "data/Day01.txt"

  def day01(): Unit = {
    val input = loadData(inputFile)
    println(s"Day01.part1 = ${part1(input)}")
    println(s"Day01.part2 = ${part2(input)}")
  }

  def part1(input : List[Int]): Int = {
    val (s, z) = input.sliding(2).foldLeft((0, 0)) { case ((s, x), l) =>
      l match {
        case a :: b :: Nil if a == b => (s+a, b)
        case a :: b :: Nil          => (s,   b)
        case _                      => (s,   0)
      }
    }

    val h = input.headOption.getOrElse(0)
    if(h == z) s + h
    else      s
  }

  def part2(input: List[Int]): Int = {
    val (x, y) = input.splitAt(input.size / 2)
    val data = input.zip(y ++ x)
    data.foldLeft(0) { case (s, (a, b)) => if(a == b) s + a else s }
  }

  def loadData(f: String): List[Int] = {
    io.Source.fromFile(f)
      .getLines().toList
      .flatten
      .map(_.asDigit)
  }
}


object Day01_B {
  val inputFile = "data/Day01.txt"

  def day01(): Unit = {
    val input = loadData(inputFile)
    println(s"Day01.part1 = ${part1(input)}")
    println(s"Day01.part2 = ${part2(input)}")
  }

  def part1(digits: List[Int]): Int = {
    val digitStream = Stream.continually(digits).flatten
    digits.zip(digitStream.drop(1))
      .collect { case (x, y) if x == y => x }
      .sum
  }

  def part2(digits: List[Int]): Int = {
    val digitStream = Stream.continually(digits).flatten
    digits.zip(digitStream.drop(digits.size/2))
      .collect { case (x, y) if x == y => x }
      .sum
  }

  def loadData(f: String): List[Int] = {
    io.Source.fromFile(f)
      .getLines.toList
      .flatten
      .map(_.asDigit)
  }

}
