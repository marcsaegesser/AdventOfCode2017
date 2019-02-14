package advent

object Day04 {
  val inputFile = "data/Day04.txt"

  type Validator = List[String] => Boolean

  def day04(): Unit = {
    val input = loadData(inputFile)
    println(s"Day04.part1 = ${part1(input)}")
    println(s"Day04.part2 = ${part2(input)}")
  }

  def part1(input: List[List[String]]): Int = {
    countValid(isValidPassphrase, input)
  }

  def part2(input: List[List[String]]): Int = {
    countValid(isValidPassphrase2, input)
  }

  def isValidPassphrase(phrase: List[String]) = phrase.distinct == phrase

  def isValidPassphrase2(phrase: List[String]) = {
    val sorted = phrase.map(_.sorted)
    sorted.distinct == sorted
  }

  def countValid(v: Validator, ps: List[List[String]]): Int =
    ps.filter(v).size

  val wordRegex = """\w+""".r

  def loadData(f: String): List[List[String]] =
    io.Source.fromFile(f)
      .getLines()
      .map(l => wordRegex.findAllIn(l).toList)
      .toList

}
