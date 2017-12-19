import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

sealed trait DanceMove {
    def apply(line: List[Char]): List[Char]
}

case class Spin(count: Int) extends DanceMove {
    override def apply(line: List[Char]): List[Char] = line.slice(line.size - count,line.size) ::: line.slice(0, line.size - count)
}

case class Exchange(a: Int, b: Int) extends DanceMove {
    override def apply(line: List[Char]): List[Char] = {
        val l = mutable.MutableList(line:_*)
        val temp = l(a)
        l(a) = l(b)
        l(b) = temp
        l.toList
    }
}

case class Partner(a: Char, b: Char) extends DanceMove {
    override def apply(line: List[Char]): List[Char] = {
        val firstPos = line.indexOf(a)
        val secondPos = line.indexOf(b)
        Exchange(firstPos, secondPos)(line)
    }
}

object MoveInstructor extends RegexParsers {
    private def number = "[0-9]{1,2}".r ^^ java.lang.Integer.parseInt
    private def name = "[a-p]".r ^^ {c => c.head}

    private def spin = "s" ~> number ^^ Spin

    private def exchange = "x" ~> (number ~ ("/" ~> number)) ^^ {
        case pos1 ~ pos2 => Exchange(pos1, pos2)
    }

    private def partner = "p" ~> (name ~ ("/" ~> name)) ^^ {
        case name1 ~ name2 => Partner(name1, name2)
    }

    private def instruction = spin | exchange | partner

    private def instructionList = instruction ~ ("," ~> instruction).* ^^ {
        case first ~ rest => first :: rest
    }

    def apply(in: String): Seq[DanceMove] = parse(instructionList, in) match {
        case Success(moves, _) => moves
        case Failure(msg, _) => throw new Exception("Parsing failed here:\n" + msg)
        case Error(msg, _) => throw new Exception("ERROR! " + msg)
    }
}

class ProgramDance(moveslist: String, toCharacter: Char = 'p') {
    private val startingPostitions = ('a' to toCharacter).toList

    private val moves = MoveInstructor(moveslist)

    private def doMoves(moves: Seq[DanceMove], positions: List[Char]): List[Char] = {
        if(moves.isEmpty) positions
        else {
            /*val instruction = moves.head
            val tempList = instruction(positions)
            println(s"from $positions with $instruction to $tempList")*/
            doMoves(moves.tail, moves.head(positions))
        }
    }

    def dance(): List[Char] = doMoves(moves, startingPostitions)

    def dance(times: Int): List[Char] = {
        //val longMoves = Stream.tabulate(times*moves.size)(n => moves(n%moves.size))

        var lastPos = startingPostitions
        for(i <- 0 until times) {
            val next = doMoves(moves, lastPos)
            if(lastPos == next) {
                println(s"found repetition after $i dances")
            }
            lastPos = next
        }

        lastPos
    }
}
