package advent

import scala.io._

object Day12 {
  type Id = Int
  type ConnectionMap = Map[Id, Set[Id]]

  def reachableFrom(id: Id, conns: ConnectionMap): Set[Id] = {
    def helper(toSearch: Set[Id], found: Set[Id]): Set[Id] =
      if(toSearch.isEmpty) found
      else {
        helper(
          toSearch.foldLeft(Set.empty[Id]){case (s, i) => s ++ conns(i) }.diff(found),
          found ++ toSearch
        )
      }

    helper(conns(id), Set.empty[Id])
  }

  def findGroups(conns: ConnectionMap): List[Set[Id]] = {
    def helper(remaining: ConnectionMap, accum: List[Set[Id]]): List[Set[Id]] =
      if(remaining.isEmpty) accum
      else {
        val g = reachableFrom(remaining.head._1, remaining)
        helper(remaining -- g, g :: accum)
      }

    helper(conns, List.empty[Set[Id]])
  }

  def mkClientConnection(s: String): (Id, Set[Id]) = {
    val (id, cs) =
      s.split("""[\s+,]""")
        .filterNot(s => s.isEmpty || s == "<->")
        .map(_.toInt)
        .splitAt(1)
    (id(0), cs.toSet)
  }

  def parseInput(input: Iterator[String]): Map[Id, Set[Id]] = {
    input.map(mkClientConnection).toMap
  }

  def loadFile(f: String): Iterator[String] =
    Source.fromFile(f).getLines
}
