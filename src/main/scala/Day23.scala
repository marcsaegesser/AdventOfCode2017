package advent

import scala.annotation._
import scala.collection.immutable.{ Set => S }

object Day23 {
  type Code = Vector[Instruction]

  sealed trait ValueType
  case class Constant(c: Long) extends ValueType { override def toString = c.toString }
  case class Register(r: Char) extends ValueType { override def toString = r.toString }

  sealed trait Instruction
  case class Set(x: Register, y: ValueType) extends Instruction
  case class Sub(x: Register, y: ValueType) extends Instruction
  case class Mul(x: Register, y: ValueType) extends Instruction
  case class Jnz(x: ValueType, y: ValueType) extends Instruction

  sealed trait ExecutableState
  case object Runnable                extends ExecutableState
  case object Break                   extends ExecutableState
  case class Crashed(reason: String)  extends ExecutableState

  sealed trait TerminationState
  case object MachineCrashed    extends TerminationState
  case object MachineDeadlocked extends TerminationState

  case class ProgramState(exeState: ExecutableState, regs: Map[Char, Long], ip: Int, mulCount: Long, bps: S[Int], wps: S[Int], code: Vector[Instruction])

  case class Machine(p0: ProgramState)

  def evalValue(v: ValueType, state: ProgramState): Long =
    v match {
      case Constant(c) => c
      case Register(r) => state.regs.getOrElse(r, 0)
    }

  @tailrec
  def eval(p: ProgramState): ProgramState =
    p.exeState match {
      case Runnable   => eval(step(p))
      case Break      => p
      case Crashed(_) => p
    }

  def step(p: ProgramState): ProgramState = {
    p match { case ProgramState(e, regs, ip, mc, bps, wps, code) =>
      val next =
        try {
          code(ip) match {
            case Set(x, y) => p.copy(regs=regs.updated(x.r, evalValue(y, p)), ip=ip+1)
            case Sub(x, y) => p.copy(regs=regs.updated(x.r, (evalValue(x, p) - evalValue(y, p))), ip=ip+1)
            case Mul(x, y) => p.copy(regs=regs.updated(x.r, (evalValue(x, p) * evalValue(y, p))), mulCount=mc+1, ip=ip+1)
            case Jnz(x, y) => if(evalValue(x, p) != 0) p.copy(ip=ip + evalValue(y, p).toInt) else p.copy(ip=ip+1)
          }
        } catch {
          case t: Throwable => p.copy(exeState=Crashed(t.getMessage))
        }
      if(bps.contains(next.ip)) next.copy(exeState=Break)
      if(wps.contains(next.ip)) { println(showRegisters(next.regs)); next }
      else                      next
    }
  }

  def evalAndPrint(p: ProgramState): ProgramState = {
    val next = step(p)
    printProgram(next)
    next
  }

  @tailrec
  def evalAndPrintN(p: ProgramState, n: Int): ProgramState = {
    if(n == 0) p
    else {
      val next = step(p)
      printProgram(next)
      Thread.sleep(200)
      evalAndPrintN(next, n-1)
    }
  }

  def addBreak(p: ProgramState, ip: Int): ProgramState =
    p.copy(bps=p.bps + ip)

  def removeBreak(p: ProgramState, ip: Int): ProgramState =
    p.copy(bps=p.bps - ip)

  def addWatch(p: ProgramState, ip: Int): ProgramState =
    p.copy(wps=p.wps + ip)

  def removeWatch(p: ProgramState, ip: Int): ProgramState =
    p.copy(wps=p.wps - ip)

  def resume(p: ProgramState): ProgramState =
    if(p.exeState == Break) p.copy(exeState=Runnable)
    else                   p

  def mkProgram(code: Code): ProgramState =
    ProgramState(Runnable, Map.empty[Char, Long], 0, 0, S.empty[Int], S.empty[Int], code)

  def showInstruction(i: Instruction): String =
    i match {
      case Set(x, y) => s"set $x $y"
      case Sub(x, y) => s"sub $x $y"
      case Mul(x, y) => s"mul $x $y"
      case Jnz(x, y) => s"jnz $x $y"
    }

  def showCode(ip: Int, code: Code): String = {
    code.zipWithIndex.map { case (i, a) =>
      if(a == ip) f"-> $a%2x:  ${showInstruction(i)}"
      else       f"   $a%2x:  ${showInstruction(i)}"
    }.mkString("\n")
  }

  def showRegisters(regs: Map[Char, Long]): String =
    ('a' to 'h').map(r => f"$r: ${regs.getOrElse(r, 0)}").mkString("  ")

  def showState(p: ProgramState): String =
    f"${p.exeState}  ${p.ip}  ${p.mulCount}"

  def showProgram(p: ProgramState): String = {
    p match { case ProgramState(s, r, ip, mulCount, bps, wps, code) =>
      showCode(ip, code) ++ "\n" ++ showRegisters(r) ++ "\n" ++ showState(p) + "\n"
    }
  }

  def printProgram(p: ProgramState): Unit = {
    print(AnsiCodes.CursorHome)
    print(showProgram(p))
  }

  def parseRegister(v: String): Register =
    if(v.head.isLetter) Register(v.head)
    else throw new Exception(s"Invalid register value $v")

  def parseValue(v: String): ValueType =
    if(v.head.isLetter) Register(v.head)
    else                Constant(v.toLong)

  def parseInstruction(s: String): Instruction = {
    val fields = s.split("""\s+""")
    (fields.head, fields.tail) match {
      case ("set", as) => Set(parseRegister(as(0)), parseValue(as(1)))
      case ("sub", as) => Sub(parseRegister(as(0)), parseValue(as(1)))
      case ("mul", as) => Mul(parseRegister(as(0)), parseValue(as(1)))
      case ("jnz", as) => Jnz(parseValue(as(0)), parseValue(as(1)))
      case (_, _)      => throw new Exception(s"Invalid code $s")
    }
  }

  def parseString(s: String): Vector[Instruction] =
    s.lines.filterNot(_.isEmpty).map(parseInstruction).toVector

  def parseFile(f: String): Vector[Instruction] =
    io.Source.fromFile(f).getLines().filterNot(_.isEmpty).map(l => parseInstruction(l.takeWhile(_ != ';'))).toVector

}

object AnsiCodes {
  // Device Status
  final val QueryDeviceCode = "\u001b[c"
  final val ReportDeviceCode = "\u001b[{code}0c"
  final val QueryDeviceStatus = "\u001b[5n"
  final val ReportDeviceOK = "\u001b[0n"
  final val ReportDeviceFailure = "\u001b[3n"
  final val QueryCursorPosition = "\u001b[6n"
//  final val ReportCursorPosition = """\u001b\[(\d+);(\d+)R""".r
  final val ReportCursorPosition = """.*\[(\d+);(\d+)R""".r

  // Terminal Setup
  final val ResetDevice = "\u001bc"
  final val EnableLineWrap = "\u001b[7h"
  final val DisableLineWrap = "\u001b[7l"

  // Cursor Control
  final def SetCursorPos(r: Int, c: Int) = s"\u001b[$r;${c}H"
  final val CursorHome = "\u001b[;H"
  final val CursorUp = "\u001b[{COUNT}A"
  final val CursorDown = "\u001b[{COUNT}B"
  final val CursorForward = "\u001b[{COUNT}C"
  final val CursorBackward = "\u001b[{COUNT}D"
  final val SaveCursor = "\u001b[s"
  final val UnsaveCursor = "\u001b[u"
  final val SaveCursorAttrs = "\u001b7"
  final val RestoreCursorAttrs = "\u001b8"

  // Erasing
  final val EraseEndofLine = "\u001b[K"
  final val EraseStartofLine = "\u001b[1K"
  final val EraseLine = "\u001b[2K"
  final val EraseDown = "\u001b[J"
  final val EraseUp = "\u001b[1J"
  final val EraseScreen = "\u001b[2J"
}
