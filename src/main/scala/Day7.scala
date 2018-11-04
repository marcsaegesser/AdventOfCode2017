package org.saegesser

object Day7 {
  sealed trait Tree[A]
  case   class LeafNode[A](value: A)                          extends Tree[A]
  case   class Node[A](value: A, children: List[Tree[A]]) extends Tree[A]

  case class Program(name: String,  weight: Int)

  case class RawData(p: Program, cs: List[String])
  case class Environment(rawData: Map[String, RawData], forest: Map[String, Tree[Program]])


  val pr = """(\w+) \((\d+)\)(?: -> (.*))*""".r
  def parseLine(string: String): RawData =
    string match {
      case pr(n, w, null) => RawData(Program(n, w.toInt), List.empty[String])
      case pr(n, w, c)    => RawData(Program(n, w.toInt), c.split(",").toList.map(_.trim))
    }

  def parseInput(input: String): Map[String, RawData] =
    input.lines
      .map(parseLine)
      .map(d => (d.p.name, d))
      .toMap

  def buildTree(rawData: Map[String, RawData]): Tree[Program] = {
    def buildTreeForName(name: String, env: Environment): Environment =
      env match {
        case Environment(rawData, forest) =>
          rawData.get(name) match {
            case Some(RawData(p, Nil)) => Environment(rawData - name, forest + (name -> LeafNode(p)))
            case Some(RawData(p, cs))  =>
              val env2 = cs.foldLeft(env) { case (e, n) => buildTreeForName(n, e) }
              val kids = cs.map(n => env2.forest(n))
              val newRawData = (rawData -- cs) - name
              val newForest = (env2.forest -- cs) + (name -> Node(p, kids))
              Environment(newRawData, newForest)
            case None => env
          }
      }

    def helper(env: Environment): Environment =
      if(env.rawData.isEmpty) env
      else                    helper(buildTreeForName(env.rawData.head._1, env))

    helper(Environment(rawData, Map.empty[String, Tree[Program]])).forest.head._2
  }

  val smallData = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""
}
