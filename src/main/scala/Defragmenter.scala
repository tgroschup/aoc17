import scala.collection.mutable

class Defragmenter(val key: String) {

    private val rowSize = 128
    private val gridSize = 128

    private def row(i: Int): IndexedSeq[Boolean] = new BadHash(key + s"-$i").getHash.map {
            case '0' => "0000"
            case '1' => "0001"
            case '2' => "0010"
            case '3' => "0011"
            case '4' => "0100"
            case '5' => "0101"
            case '6' => "0110"
            case '7' => "0111"
            case '8' => "1000"
            case '9' => "1001"
            case 'a' => "1010"
            case 'b' => "1011"
            case 'c' => "1100"
            case 'd' => "1101"
            case 'e' => "1110"
            case 'f' => "1111"
        }.flatten.map(_ == '1')

    private val rows: IndexedSeq[IndexedSeq[Boolean]] = (0 to gridSize).map(row)

    def printHashDot: String = {
        val header: String = "visualizing an " + rows.size + "x" + rows(0).size + " grid"

        def rowToHashDot(r: IndexedSeq[Boolean]): String = r.foldRight(""){case (bit, string) => {if(bit) '#' else '.'}+string}

        rows.foldRight(header + "\n"){case (row, string) => rowToHashDot(row)+"\n"+string}
    }

    val count: Int = rows.flatten.map(if(_) 1 else 0).sum

    def countConnectedComponents: Int = {
        case class Coord(x: Int, y: Int) {
            def toIndex: Int = x + y * rowSize

            def neighbours: Seq[Coord] = Seq(Coord(x+1, y), Coord(x-1, y), Coord(x, y+1), Coord(x, y-1))
                                        .filter(c => c.x >= 0 && c.x < gridSize && c.y >=0 && c.y < rowSize)
        }

        val visited = Array.fill[Boolean](rowSize*gridSize)(false)

        var components = 0

        def dfs(c: Coord): Unit = {
            for(n <- c.neighbours if !visited(n.toIndex) && rows(n.x)(n.y)) {
                visited(n.toIndex) = true
                dfs(n)
            }
        }

        val coords = (0 until gridSize).zip(0 until rowSize).map(c => Coord(c._1, c._2))

        for(c <- coords if !visited(c.toIndex) && rows(c.x)(c.y)) {
            visited(c.toIndex) = true
            dfs(c)
            components += 1
            println(s"component $components started at $c")
        }

        components
    }
}
