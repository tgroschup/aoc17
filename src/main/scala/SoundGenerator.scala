import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

class SoundRegfile {
    private val r = mutable.Map[String, Long]().withDefaultValue(0)

    def apply(key: String): Long = r(key)

    def set(key: String, newVal: Long): Unit = r.update(key, newVal)

    override def toString: String = r.toString()
}

sealed trait SoundInstruction {
    def execute(implicit regFile: SoundRegfile): Long
}
sealed trait SIFlowControl extends SoundInstruction
sealed trait SIExecute extends SoundInstruction
case class Snd(regName: String) extends SIFlowControl {
    override def execute(implicit regFile: SoundRegfile): Long = regFile(regName)
}
case class Set(regName: String, value: Either[Int, String]) extends SIExecute {
    override def execute(implicit regFile: SoundRegfile): Long = {
        val newVal: Long = if(value.isLeft){
            value.left.get
        } else {
            val otherReg = value.right.get
            regFile(otherReg)
        }
        regFile.set(regName, newVal)
        newVal
    }
}
case class Add(regName: String, value: Either[Int, String]) extends SIExecute {
    override def execute(implicit regFile: SoundRegfile): Long = {
        val add: Long = if(value.isLeft){
            value.left.get
        } else {
            val otherReg = value.right.get
            regFile(otherReg)
        }
        val newVal = add + regFile(regName)
        regFile.set(regName, newVal)
        newVal
    }
}
case class Mul(regName: String, value: Either[Int, String]) extends SIExecute {
    override def execute(implicit regFile: SoundRegfile): Long = {
        val mult: Long = if(value.isLeft){
            value.left.get
        } else {
            val otherReg = value.right.get
            regFile(otherReg)
        }
        val newVal = mult * regFile(regName)
        regFile.set(regName, newVal)
        newVal
    }
}
case class Mod(regName: String, value: Either[Int, String]) extends SIExecute {
    override def execute(implicit regFile: SoundRegfile): Long = {
        val div: Long = if(value.isLeft){
            value.left.get
        } else {
            val otherReg = value.right.get
            regFile(otherReg)
        }
        val newVal = regFile(regName)%div
        regFile.set(regName, newVal)
        newVal
    }
}
case class Rcv(regName: String) extends SoundInstruction {
    override def execute(implicit regFile: SoundRegfile): Long = throw new Exception(s"RCv should never be executed...")
}
case class Jgz(condition: Either[Int, String], value: Either[Int, String]) extends SIFlowControl {
    override def execute(implicit regFile: SoundRegfile): Long = {
        val offset: Long = if(value.isLeft){
            value.left.get
        } else {
            val otherReg = value.right.get
            regFile(otherReg)
        }

        val cond = if(condition.isLeft){
            condition.left.get
        } else {
            val otherReg = condition.right.get
            regFile(otherReg)
        }
        if(cond > 0) offset else 1
    }
}

object SoundinstructionParser extends RegexParsers{
    private def name = "[a-z]".r
    private def number = "-?[0-9]+".r ^^ {java.lang.Integer.parseInt}

    private def nameOrNumber = (name | number) ^^ {
        case name: String => Right(name)
        case number: Int =>  Left(number)
    }

    private def snd = "snd" ~> name ^^ Snd
    private def rcv = "rcv" ~> name ^^ Rcv
    private def set = "set" ~> name ~ nameOrNumber ^^ {case reg ~ freq => Set(reg, freq)}
    private def add = "add" ~> name ~ nameOrNumber ^^ {case reg ~ freq => Add(reg, freq)}
    private def mul = "mul" ~> name ~ nameOrNumber ^^ {case reg ~ freq => Mul(reg, freq)}
    private def mod = "mod" ~> name ~ nameOrNumber ^^ {case reg ~ freq => Mod(reg, freq)}
    private def jgz = "jgz" ~> nameOrNumber ~ nameOrNumber ^^ {case reg ~ freq => Jgz(reg, freq)}

    private def instructions = (snd | rcv | set | add | mul | mod | jgz).+

    def apply(input: String): IndexedSeq[SoundInstruction] = {
        parse(instructions, input) match {
            case Success(res, _) => res.toIndexedSeq
            case Failure(msg, _) => throw new Exception("Parsing failed here:\n" + msg)
            case Error(msg, _) => throw new Exception("ERROR! " + msg)
        }
    }
}


class SoundGenerator(instructions: String) {
    val commands = SoundinstructionParser(instructions)

    implicit val regFile: SoundRegfile = new SoundRegfile()

    println(s"instructions(${commands.length}): " + commands.toString)

    private def executeCommand(c: IndexedSeq[SoundInstruction], IP: Int = 0, lastPlayed: Long = 0)(implicit regFile: SoundRegfile): Long =
        if(IP >= c.size || IP < 0) lastPlayed
        else {
            val instr = c(IP)

            //println(s"intr(ip: $IP): $instr on register: $regFile")

            val nextIP: Int = instr match {
                case jump: Jgz => IP + jump.execute.toInt
                case _ => IP + 1
            }

            val played: Long = instr match {
                case play: Snd => play.execute
                case _ => lastPlayed
            }

            instr match {
                case ex: SIExecute => ex.execute
                case recover: Rcv => return lastPlayed
                case f: SIFlowControl =>
            }

            executeCommand(c, nextIP, played)

        }

    def getFirstRecovered: Long = executeCommand(commands)
}
