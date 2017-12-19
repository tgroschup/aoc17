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

    private val allMoves = MoveInstructor(moveslist)

    private def doMoves(moves: Seq[DanceMove], positions: List[Char], doneMoves: Int = 0): List[Char] = {
        if(moves.isEmpty) positions
        else {
            /*val instruction = moves.head
            val tempList = instruction(positions)
            println(s"from $positions with $instruction to $tempList")*/
            doMoves(moves.tail, moves.head(positions), doneMoves + 1)
        }
    }

    private def findLoop(last: List[Char], counter: Int = 1): Int = {
        val next = doMoves(allMoves, last)
        if(next == startingPostitions) counter
        else findLoop(next, counter + 1)
    }

    val loopSize: Int = findLoop(startingPostitions)

    def dance(): List[Char] = doMoves(allMoves, startingPostitions)

    def dance(times: Int): List[Char] = {
        val todoMoves = times % loopSize

        println(s"found loop after $loopSize complete dances: only doing $todoMoves dances")

        var lastMove = startingPostitions
        for(_ <- 0 until todoMoves) {
           lastMove = doMoves(allMoves, lastMove)
        }

        lastMove
    }
}
