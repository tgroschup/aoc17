import scala.collection.mutable

class Reallocator(startFilling: String) {
    private val banks: Array[Int] = startFilling.split("\\s+").map(java.lang.Integer.parseInt)

    private val seenStates = mutable.MutableList[List[Int]]()

    private var lastState: List[Int] = List()

    private def redistribute(banks: Array[Int], startIndex: Int): Unit = {
        def redist(current: Array[Int], index: Int, rest: Int): Unit = if (rest != 0) {
            current(index) += 1
            redist(current, (index+1)%current.length, rest-1 )
        }

        val blocks = banks(startIndex)
        banks(startIndex) = 0

        redist(banks, (startIndex+1)%banks.length, blocks)
    }

    private def findLoop(currentBanks: Array[Int], counter: Int = 1): Int = {
        seenStates += currentBanks.toList
        val startIndex = currentBanks.indexOf(currentBanks.max)
        redistribute(currentBanks, startIndex)
        if(seenStates.contains(currentBanks.toList)) {
            lastState = currentBanks.toList
            counter
        }
        else findLoop(currentBanks, counter + 1)
    }

    def countRedistributions: Int = findLoop(banks.clone)

    def getLoopSize: Int = {
        if(lastState.isEmpty) {
            countRedistributions
        }
        seenStates.size - seenStates.indexOf(lastState)
    }
}
