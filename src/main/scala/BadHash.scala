import scala.collection.immutable.Queue

class BadHash(val input: String) {
    val suffix: List[Int] = List(17, 31, 73, 47, 23)

    case class Round(list: List[Int], position: Int, skip: Int)

    private def doRound(list: List[Int], lengths: Queue[Int], pos: Int, skip: Int): Round = if(lengths.nonEmpty) {
        val size = list.size

        val (length, restLengths) = lengths.dequeue

        val end = pos + length

        val circularExtendedList = list ::: list
        val reversedSlice = circularExtendedList.slice(pos, end).reverse
        val circularReversed = circularExtendedList.slice(0,pos) ::: reversedSlice ::: circularExtendedList.slice(end, circularExtendedList.size)

        val beginning = circularReversed.slice(size, end)
        val rest = circularReversed.slice(beginning.size, size)

        doRound(beginning ::: rest, restLengths, (pos + length + skip) % size , skip + 1)
    } else {
        Round(list, pos, skip)
    }

    def transform(size: Int): Int = {
        val lengths: List[Int] = input.split(",").map(java.lang.Integer.parseInt).toList
        val t = doRound((0 until size).toList, Queue(lengths: _ *), 0, 0).list

        t.head * t(1)
    }

    def getHash: String = {
        val asciiBytes: List[Int] = input.map(_.toInt).toList

        var lastRound: Round = Round((0 until 255).toList, 0, 0)

        for(i <- 0 to 64) {
            lastRound = doRound(lastRound.list, Queue(asciiBytes:::suffix:_*), lastRound.position, lastRound.skip)
        }

        val sparseHash = lastRound.list
        val denseHash = sparseHash.grouped(16).map(block => block.foldRight(block.head){
            case (entry, xord) => entry ^ xord
        })

        denseHash.foldRight(""){case (number, string) => number.toHexString + string}

    }
}
