package advent

object Day02 {
  type Matrix = List[List[Int]]

  def day02(): Unit = {
    val input = loadData(inputFile)
    println(s"Day02.part1 = ${part1(input)}")
    println(s"Day02.part2 = ${part2(input)}")
  }

  def part1(input: Matrix): Int = {
    def maxDiff(l: List[Int]): Int = l.max - l.min

    input.map(maxDiff).sum
  }

  def part2(input: List[List[Int]]): Int = {
    input
      .map { l =>
        l.combinations(2)
          .map { l => l.sorted }
          .collect { case a :: b :: Nil if b%a == 0 => b/a }
          .next  }
      .sum
  }

  def part2b(input: List[List[Int]]): Int = {
    input
      .map { l =>
        l
          .sorted
          .combinations(2)
          .foldLeft(0) {
            case (r, a :: b :: Nil) if b%a == 0 => b/a
            case (r, _)                        => r
          }
      }
      .sum
  }

  val inputFile = "data/Day02.txt"

  def loadData(f: String): List[List[Int]] =
    io.Source.fromFile(f).getLines()
      .map(_.split("\\s+").map(_.toInt).toList)
      .toList

}
