package org.saegesser

import scala.io._

object Day18 {
  sealed trait Instruction
  case class Snd(x: String)            extends Instruction
  case class Set(x: String, y: String) extends Instruction
  case class Add(x: String, y: String) extends Instruction
  case class Mul(x: String, y: String) extends Instruction
  case class Mod(x: String, y: String) extends Instruction
  case class Rcv(x: String)            extends Instruction
  case class Jgz(x: String, y: String) extends Instruction

  sealed trait ExecutableState
  case object Runnable extends ExecutableState
  case object Waiting  extends ExecutableState
  case object Crashed  extends ExecutableState

  sealed trait TerminationState
  case object MachineCrashed    extends TerminationState
  case object MachineDeadlocked extends TerminationState

  case class ProgramState(exeState: ExecutableState, regs: Map[Char, Long], snd: Vector[Long], rcv: Vector[Long], ip: Long, sndCount: Long, code: Vector[Instruction])

  case class Machine(p0: ProgramState, p1: ProgramState)

  def evalValue(v: String, state: ProgramState): Long = {
    if(v.head.isLetter) state.regs.getOrElse(v.head, 0)
    else                v.toLong
  }

  def eval(p: ProgramState): ProgramState = {
    p match { case ProgramState(e, regs, snd, rcv, ip, sc, code) =>
      try {
        code(ip.toInt) match {
          case Snd(x)    => eval(p.copy(snd=(snd :+ evalValue(x, p)), sndCount=sc+1, ip=ip+1))
          case Set(x, y) => eval(p.copy(regs=regs.updated(x.head, evalValue(y, p)), ip=ip+1))
          case Add(x, y) => eval(p.copy(regs=regs.updated(x.head, (evalValue(x, p) + evalValue(y, p))), ip=ip+1))
          case Mul(x, y) => eval(p.copy(regs=regs.updated(x.head, (evalValue(x, p) * evalValue(y, p))), ip=ip+1))
          case Mod(x, y) => eval(p.copy(regs=regs.updated(x.head, (evalValue(x, p) % evalValue(y, p))), ip=ip+1))
          case Jgz(x, y) => eval(if(evalValue(x, p) > 0) p.copy(ip=ip + evalValue(y, p)) else p.copy(ip=ip+1))
          case Rcv(x)    =>
            if(rcv.isEmpty) p.copy(exeState=Waiting)
            else            eval(p.copy(regs=regs.updated(x.head, rcv.head), rcv=rcv.tail, ip=ip+1))
        }
      } catch {
        case t: Throwable => p.copy(exeState=Crashed)
      }
    }
  }

  def updateBuffers(m: Machine): Machine = {
    m match { case Machine(p0, p1) =>
      val snd0 = p0.snd
      Machine(
        p0.copy(rcv=p0.rcv++p1.snd, snd=Vector.empty[Long]),
        p1.copy(rcv=p1.rcv++snd0, snd=Vector.empty[Long])
      )
    }
  }

  def runMachine(m: Machine): (Machine, TerminationState) = {
    updateBuffers(m) match {
      case Machine(p0, p1) if p0.exeState==Crashed  || p0.exeState==Crashed                     => (m, MachineCrashed)
      case Machine(p0, p1) if p0.exeState==Runnable || (p0.exeState==Waiting && !p0.rcv.isEmpty) => runMachine(Machine(eval(p0), p1))
      case Machine(p0, p1) if p1.exeState==Runnable || (p1.exeState==Waiting && !p1.rcv.isEmpty) => runMachine(Machine(p0, eval(p1)))
      case _                                                                                 => (m, MachineDeadlocked)
    }
  }

  def mkMachine(code: Vector[Instruction]): Machine = {
    Machine(
      ProgramState(Runnable, Map('p'->0), Vector.empty[Long], Vector.empty[Long], 0, 0, code)
    , ProgramState(Runnable, Map('p'->1), Vector.empty[Long], Vector.empty[Long], 0, 0, code)
    )
  }

  def parseInstruction(s: String): Instruction = {
    val fields = s.split("""\s+""")
    (fields.head, fields.tail) match {
      case ("snd", as) => Snd(as(0))
      case ("set", as) => Set(as(0), as(1))
      case ("add", as) => Add(as(0), as(1))
      case ("mul", as) => Mul(as(0), as(1))
      case ("mod", as) => Mod(as(0), as(1))
      case ("rcv", as) => Rcv(as(0))
      case ("jgz", as) => Jgz(as(0), as(1))
      case (_, _)      => throw new Exception(s"Invalid code $s")
    }
  }

  def parseString(s: String): Vector[Instruction] =
    s.lines.filterNot(_.isEmpty).map(parseInstruction).toVector

  def parseFile(f: String): Vector[Instruction] =
    Source.fromFile(f).getLines().filterNot(_.isEmpty).map(parseInstruction).toVector

  val sampleCode = """
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""
}

/*
 snd X plays a sound with a frequency equal to the value of X.
set X Y sets register X to the value of Y.
add X Y increases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)*/
