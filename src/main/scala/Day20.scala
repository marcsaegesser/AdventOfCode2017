package advent

object Day20 {

  case class Triple(x: BigInt, y: BigInt, z: BigInt) {
    def +(other: Triple): Triple = Triple(this.x + other.x, this.y + other.y, this.z + other.z)
    def mag: BigInt = x.abs + y.abs + z.abs
  }
  case class Particle(id: Int, p: Triple, v: Triple, a: Triple) {
    lazy val nextState: Particle = {
      val vp = v + a
      this.copy(p=p+vp, v=vp)
    }
    def dist: BigInt = p.mag
    def dirEstablished: Boolean =
      (v.x.signum == a.x.signum || a.x.signum == 0) &&
      (v.y.signum == a.y.signum || a.y.signum == 0) &&
      (v.z.signum == a.z.signum || a.z.signum == 0)

    def dirOut: Boolean = nextState.dist - this.dist > 0
  }

  def updateParticles(ps: Vector[Particle]): Vector[Particle] = {
    ps.map(_.nextState)
  }

  def removeCollisions(ps: Vector[Particle]): Vector[Particle] = {
    ps
      .groupBy(_.p)
      .filter { case (k, v) => v.size == 1 }
      .values.toVector.flatten

  }


  def filterParticles(ps: Vector[Particle]): Vector[Particle] = {
    val data =
      ps
        .map(p => (p, p.dist, p.dirOut, p.dirEstablished))
        .sortBy(_._2)

    val p0 = data(0)._1
    data
      .filterNot { case (p, d, o, e) => o && e && d > p0.dist }
      .map(_._1)
  }

  def runSimulation(ps: Vector[Particle]): Particle = {
    val x = ps.sortBy(_.dist)
    if(x.tail.exists(p => !(p.dirOut && p.dirEstablished))) runSimulation(x.map(_.nextState))
    else x(0)
  }

  def runSim1N(num: Int, ps: Vector[Particle]): Vector[Particle] = {
    if(ps.size == 1 || num==0) ps
    else runSim1N(num-1, updateParticles(ps))
  }

  def runSimN(num: Int, ps: Vector[Particle]): Vector[Particle] = {
    if(ps.size == 1 || num==0) ps
    else runSimN(num-1, removeCollisions(updateParticles(ps)))
  }

  def parseTriple(s: String): Triple = {
    val fields = s.split(",").map(_.trim.toLong)
    Triple(fields(0), fields(1), fields(2))
  }

  val particleRegex = """p=<(.+)>,\s*v=<(.+)>,\s*a=<(.+)>""".r
  val parseParticle = (s: String, i: Int) => {
    val particleRegex(p, v, a) = s
    Particle(i, parseTriple(p), parseTriple(v), parseTriple(a))
  }

  def parseFile(f: String): Vector[Particle] =
    io.Source.fromFile(f)
      .getLines()
      .filterNot(_.isEmpty)
      .zipWithIndex
      .map{ parseParticle.tupled }
      .toVector
}
