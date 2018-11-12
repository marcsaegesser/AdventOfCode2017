package org.saegesser

object Day21 {
  type Rules = Map[String, String]
  type Grid = Vector[Vector[Char]]

  def iterateN(g: Grid, n: Int, rules: Rules): Grid =
    if(n == 0) g
    else iterateN(iterate(g, rules), n-1, rules)

  def iterate(g: Grid, rules: Rules): Grid =
    if(g.size % 2 == 0) replaceGrid(g, 2, rules)
    else replaceGrid(g, 3, rules)

  def replaceGrid(g: Grid, n: Int, rules: Rules): Grid = {
    combineGrid(divideGrid(g, n).map(_.map(g => findRule(rules, g))))
  }

  def divideGrid(g: Grid, n: Int): Vector[Vector[Grid]] =
    g.sliding(n, n).toVector.map(_.transpose.sliding(n, n).toVector.map(_.transpose))

  def combineGrid(gs: Vector[Vector[Grid]]): Grid =
    gs.map(_.transpose.map(_.flatten)).flatten

  def findRule(rules: Rules, g: Grid): Grid =
    gridFromString(allForms(g).map(gridToString).collect(rules).head)

  def gridSize(g: Grid): Int = g.size

  def flipVert(g: Grid): Grid =
    g.reverse

  def flipHoriz(g: Grid): Grid =
    g.map(_.reverse)

  def rotate(g: Grid): Grid =
    g.map(_.reverse).transpose

  def allForms(g: Grid): List[Grid] = {
    def allFlips(gg: Grid): List[Grid] =
      List(flipVert(gg), flipHoriz(gg))

    val rs = List(g, rotate(g), rotate(rotate(g)), rotate(rotate(rotate(g))))
    (rs ++ rs.map(allFlips).flatten).distinct
  }

  def countGrid(g: Grid): Int =
    g.flatten.filter(_ == '#').size

  def gridFromString(s: String): Grid =
    s.split("/").map(_.toVector).toVector

  def gridToString(g: Grid): String =
    g.map(_.mkString).mkString("/")

  def showGrid(g: Grid): String =
    g.map(_.mkString).mkString("\n")

  val rulesRegex = """(.*)\s+=>\s+(.*)""".r
  def parseRule(s: String): (String, String) = {
    val rulesRegex(a, b) = s
    (a, b)
  }

  def parseRules(f: String): Rules =
    io.Source.fromFile(f)
      .getLines()
      .filterNot(_.isEmpty)
      .map(parseRule)
      .toMap


  val exampleGridString = "##./#../..."
  val initialGridString = ".#./..#/###"
}
