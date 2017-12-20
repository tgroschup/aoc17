import scala.annotation.tailrec
import scala.collection.mutable


class Stormlock(val steps: Int) {
    private val cBuffer =  mutable.ListBuffer(0)

    @tailrec
    private def update(value: Int, times: Int, position: Int = 0): Int = if(times > 0) {
        val i = (position + steps)%cBuffer.size + 1

        //println(s"insert $value at position $i into $cBuffer")

        cBuffer.insert(i, value)

        update(value + 1, times -1, i)

    } else {
        cBuffer((position + 1 )% cBuffer.size)
    }

    @tailrec
    private def calcSecondPosition(value: Int, times: Int, position: Int, virtualSize: Int, valueAt2nd: Int): Int = if(times > 0) {
        val nextPosition = (position + steps) % virtualSize + 1
        val newValueAt2nd = if(nextPosition == 1) value else valueAt2nd
        //println(s"value: $value, nextPos: $nextPosition, vSize: $virtualSize, 2nd Pos: $newValueAt2nd")
        calcSecondPosition(value + 1, times - 1, nextPosition, virtualSize + 1, newValueAt2nd)
    }  else {
        valueAt2nd
    }

    def runBuffer(times: Int): Int = update(1, times)

    def currentBuffer: List[Int] = cBuffer.toList

    def secondValueAfter(times: Int): Int = calcSecondPosition(1, times, 0, 1, 0)
}

