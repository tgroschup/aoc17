import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class RegisterFile {
    private val registers = mutable.Map[String, Int]()

    private var _allTimeMax = 0

    def apply(register: String): Int = registers.getOrElseUpdate(register, 0)

    def add(register: String, value: Int): Unit =
        if(registers.contains(register)) {
            registers(register) += value
            val newVal = registers(register)
            if(newVal > _allTimeMax) _allTimeMax = newVal
        } else {
            if(_allTimeMax < value) _allTimeMax = value
            registers(register) = value
        }

    def max: Int = registers.maxBy(_._2)._2

    def allTimeMax: Int = _allTimeMax

    override def toString: String = registers.toString
}


class RegisterInstructionParser(input: String) extends RegexParsers {
    private val regfile = new RegisterFile

    case class RegisterAdd(name: String, value: Int)

    case class Comp(name: String, value: Int, op: (Int, Int) => Boolean) {
        def eval: Boolean = {
            val regEntry = regfile(name)
            op(regEntry, value)
        }
    }

    def registerName: Parser[String] = "[a-z]+".r
    def value: Parser[Int] = "-?[0-9]+".r  ^^ {java.lang.Integer.parseInt}

    def decrement: Parser[RegisterAdd] = registerName ~ ("dec" ~> value) ^^ {case name ~ value => RegisterAdd(name, -1 * value)}
    def increment: Parser[RegisterAdd] = registerName ~ ("inc" ~> value) ^^ {case name ~ value => RegisterAdd(name, value)}

    def condition: Parser[Comp] = "if" ~> registerName ~ ("==" | ">=" | "<=" | ">" | "<" | "!=") ~ value ^^ {
        case name ~ op ~ value => op match {
            case "==" => Comp(name, value, _ == _)
            case ">" => Comp(name, value, _ > _)
            case "<" => Comp(name, value, _ < _)
            case ">=" => Comp(name, value, _ >= _)
            case "<=" => Comp(name, value, _ <= _)
            case "!=" => Comp(name, value, _ != _)
        }
    }

    def registerManipulationStatement: Parser[Unit] = (increment | decrement) ~ condition ^^ {
        case RegisterAdd(name, value) ~ condition =>
            if(condition.eval) regfile.add(name, value)
    }

    def registerManipulations: Parser[List[Unit]] = registerManipulationStatement.+

    def execute: RegisterFile = {
        parse(registerManipulations, input) match {
            case Success(_, _) => regfile
            case Failure(msg, _) => throw new Exception("Parsing failed here:\n" + msg)
            case Error(msg, _) => throw new Exception("ERROR! " + msg)
        }
    }
}