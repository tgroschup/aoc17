object Coordinate {
    def fromSpiralCoordinate(position: Int): Coordinate = {
        val x = 0
        val y = 0
        Coordinate(x, y)
    }
}

case class Coordinate(x: Int, y: Int) {
    def mannheimDistanceFromOrigin: Int = x + y
}
