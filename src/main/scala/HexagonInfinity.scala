import scala.collection.mutable

sealed trait HexDirection
case object N  extends HexDirection
case object NE extends HexDirection
case object SE extends HexDirection
case object S  extends HexDirection
case object SW extends HexDirection
case object NW extends HexDirection
object HexDirection {
    def apply(name: String): HexDirection = name match {
        case "n" => N
        case "ne" => NE
        case "se" => SE
        case "s" => S
        case "sw" => SW
        case "nw" => NW
    }
}

case class AxialCoord(q: Int, r: Int) {
    def next(d: HexDirection): AxialCoord = d match {
        case N  => AxialCoord(q, r-1)
        case NE => AxialCoord(q+1, r-1)
        case SE => AxialCoord(q+1, r)
        case S  => AxialCoord(q, r+1)
        case SW => AxialCoord(q-1, r+1)
        case NW => AxialCoord(q-1, r)
    }

    def distance(o: AxialCoord): Int =  (math.abs(q - o.q) + math.abs(q + r - o.q - o.r) + math.abs(r - o.r)) / 2
}

object Hexagon {
    private val hexagons = mutable.Map[AxialCoord, Hexagon]()

    def apply(coord: AxialCoord): Hexagon = hexagons.getOrElseUpdate(coord, new Hexagon(coord))
}
case class Hexagon(coord: AxialCoord) {
    private val neighbours = mutable.Map[HexDirection, Hexagon]()

    def distance(other: Hexagon): Int = coord.distance(other.coord)

    def goTo(d: HexDirection): Hexagon = neighbours.getOrElseUpdate(d, Hexagon(coord.next(d)))
}


class HexagonInfinity(input: String) {
    private val directionList: Seq[HexDirection] = input.split(",").map(HexDirection(_))

    private def walkPath(path: Seq[HexDirection], current: Hexagon): Hexagon =
        if(path.nonEmpty) walkPath(path.tail, current.goTo(path.head))
        else current

    def distanceAfterWalk: Int = {
        val origin = Hexagon(AxialCoord(0,0))
        origin.distance(walkPath(directionList, origin))
    }
}
