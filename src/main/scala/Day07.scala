package advent

object Day07 {
  case class Tree[A](value: A, note: Int, children: List[Tree[A]])

  type Name = String
  type Weight = Int

  case class Program(name: Name,  weight: Weight)

  case class RawData(p: Program, cs: List[Name])

  case class Environment(rawData: Map[Name, RawData], forest: Map[Name, Tree[Program]])

  def day07(): Unit = {
    val input = loadData(inputFile)
    println(s"Day07.part1 = ${part1(input)}")
    println(s"Day07.part2 = ${part2(input)}")
  }

  def part1(input: Map[Name, RawData]): String = {
    buildTree(input).value.name
  }

  def part2(input: Map[Name, RawData]): Weight = {
    val tree = buildTree(input)
    val (name, delta) = findImbalance(tree, tree.note)
    input(name).p.weight + delta
  }

  val parseRaw = """(\w+) \((\d+)\)(?: -> (.*))*""".r
  val wordRegex = """\w+""".r

  def parseLine(string: String): RawData =
    string match {
      case parseRaw(n, w, null) => RawData(Program(n, w.toInt), List.empty[String])
      case parseRaw(n, w, c)    => RawData(Program(n, w.toInt), wordRegex.findAllIn(c).toList)
    }

  val inputFile = "data/Day07.txt"

  def loadData(f: String): Map[String, RawData] =
    io.Source.fromFile(f).getLines
      .map(parseLine)
      .map(d => (d.p.name, d))
      .toMap

  def buildTree(rawData: Map[Name, RawData]): Tree[Program] = {
    def buildTreeForName(env: Environment, name: Name): Environment =
      env match { case Environment(rawData, forest) =>
        rawData.get(name) match {
          case None => env                // This program is already in the forest
          case Some(RawData(p, cs))  =>   // Build the tree for this program and its children
            val env2 = cs.foldLeft(env) { case (e, n) => buildTreeForName(e, n) }
            val kids = cs.map(n => env2.forest(n))
            val kidsWeight = kids.map(_.note).sum
            val newRawData = (env2.rawData -- cs) - name
            val newForest  = (env2.forest -- cs) + (name -> Tree(p, kidsWeight + p.weight, kids))
            Environment(newRawData, newForest)
        }
      }

    def helper(env: Environment): Environment =
      if(env.rawData.isEmpty) env
      else                    helper(buildTreeForName(env, env.rawData.head._1))

    helper(Environment(rawData, Map.empty[Name, Tree[Program]])).forest.head._2
  }

  def findImbalance(tree: Tree[Program], target: Weight): (String, Int) = {
    tree match {
      case Tree(v, n, cs) if cs.size >= 3 =>
        val x = cs.sortBy(_.note)
        if(x(0).note == x(1).note)
          if(x(0).note == x.last.note) (v.name, target - n)
          else                        findImbalance(x.last, x(0).note)
        else findImbalance(x(0), x(1).note)
      case Tree(v, n, cs) if cs.size == 2 => ???
      case Tree(v, n, cs) if cs.size == 1 => ???
      case Tree(v, n, cs)                => (v.name, target - n)
    }
  }
}
