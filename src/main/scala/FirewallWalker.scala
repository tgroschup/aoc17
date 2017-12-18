import scala.collection.mutable

case class FirewallLayer(level: Int, range: Int) {
    private var currentScannerPosition = 1
    private var goingDown = true
    def getPenalty: Int = if(range != 0 && currentScannerPosition == 1) level * range else 0
    def doAPicosecond(): Unit = if(range != 0) {
        if (goingDown && currentScannerPosition < range) {
            currentScannerPosition += 1
        } else if (goingDown && currentScannerPosition == range) {
            currentScannerPosition -= 1
            goingDown = false
        } else if (!goingDown && currentScannerPosition > 1) {
            currentScannerPosition -= 1
        } else if (!goingDown && currentScannerPosition == 1) {
            currentScannerPosition += 1
            goingDown = true
        } else {
            throw new Exception(s"illegal layer configuration: $currentScannerPosition and goingDown $goingDown")
        }
    }

    //reuse class without full simulation
    def detected(delay: Int): Boolean = (level + delay) % (2 * range - 2) == 0
}

class Paket {
    private var currentPosition: Int = -1
    private var currentPenaltyScore: Int = 0

    def increasePenalty(inc: Int): Unit = currentPenaltyScore += inc
    def penalty: Int = currentPenaltyScore

    def position: Int = currentPosition
    def doAPicosecond(): Unit = currentPosition += 1
}

class FirewallWalker(input: String) {
    private val firewall: Seq[FirewallLayer] =  makeFirewall(input)

    private val activeLayers = makeActiveFirewall(input)

    private def makeFirewall(in: String): Seq[FirewallLayer] = {
        val tempWall: mutable.MutableList[FirewallLayer] = mutable.MutableList()

        val lines = in.split("\n")

        for(line <- lines) {
            val levelAndRange = line.split(": ")

            if(levelAndRange.length != 2) {
                throw new Exception("Pasring error in line " + lines.indexOf(line)
                     +"\nline lookes like:\n" + line)
            }

            val level = java.lang.Integer.parseInt(levelAndRange(0))
            var lastLevel = if(tempWall.nonEmpty) tempWall.last.level else -1
            while(level != lastLevel + 1) {
               tempWall += FirewallLayer(lastLevel + 1, 0)
                lastLevel += 1
            }


            tempWall += FirewallLayer(level,java.lang.Integer.parseInt(levelAndRange(1)))
        }

        tempWall
    }

    private def makeActiveFirewall(in: String): Seq[FirewallLayer] = {
        val tempWall: mutable.MutableList[FirewallLayer] = mutable.MutableList()

        val lines = in.split("\n")

        for(line <- lines) {
            val levelAndRange = line.split(": ")

            if(levelAndRange.length != 2) {
                throw new Exception("Pasring error in line " + lines.indexOf(line)
                    +"\nline lookes like:\n" + line)
            }

            val level = java.lang.Integer.parseInt(levelAndRange(0))
            tempWall += FirewallLayer(level,java.lang.Integer.parseInt(levelAndRange(1)))
        }

        tempWall
    }

    private def doAPicosencond(p: Paket): Int = {
        p.doAPicosecond()
        val currentLayer = p.position
        p.increasePenalty(firewall(currentLayer).getPenalty)
        firewall.foreach(_.doAPicosecond())
        p.penalty
    }

    def walkThrough: Int = {
        val currentPaket = new Paket

        while(currentPaket.position < firewall.length-1) {
            doAPicosencond(currentPaket)
        }

        currentPaket.penalty
    }

    private def walkToFirstPenalty(): Int = {
        def walk(p: Paket): Int = if(p.position == firewall.length - 1) 0
        else {
            val currentPenalty = doAPicosencond(p)
            if(currentPenalty == 0) walk(p)
            else currentPenalty
        }

        walk(new Paket)
    }

    def delayStart(pSec: Int): Int = {
        for(_ <- 0 until pSec)
            firewall.foreach(_.doAPicosecond())
        walkToFirstPenalty()
    }

    def getMinDelayForZeroPenalty: Int = {
        var delay: Int = 10
        var lastPenalty: Int = 1

        while (lastPenalty!=0) {
            lastPenalty = delayStart(delay)

            if(delay%1000 == 0) {
                println("one thousand ps later")
            }
            delay += 1
        }
        delay - 1
    }

    private def notDetected(delay: Int) : Boolean = activeLayers.forall(!_.detected(delay))

    def minDelayUndetected: Int = Stream.from(0).find(notDetected).get
}
