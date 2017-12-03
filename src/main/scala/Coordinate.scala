import scala.collection.mutable

object Coordinate {
    type ValueMap = mutable.Map[Coordinate, Option[Int]]

    val m: ValueMap = mutable.Map(Coordinate(0,0) -> Some(1))

    private def updateValueMap(c: Coordinate): Unit = {

        val surroundingValues: List[Option[Int]] = List(m.getOrElse(c.right, None), m.getOrElse(c.up, None)
            , m.getOrElse(c.left, None), m.getOrElse(c.down, None), m.getOrElse(c.right.up, None)
            , m.getOrElse(c.up.left, None), m.getOrElse(c.left.down, None), m.getOrElse(c.down.right, None))

        val newVal: Option[Int] = Some(surroundingValues.map(_.getOrElse(0)).sum)

        m += c -> newVal
    }

    private def goUp(c: Coordinate, currentSize: Int, currentPosition: Int, toPosition: Int): Coordinate = {
        updateValueMap(c)
        if(currentPosition == toPosition) c
        else if(c.isRightUpCorner(currentSize)) goLeft(c.left, currentSize, currentPosition+1, toPosition)
        else goUp(c.up, currentSize, currentPosition+1, toPosition)
    }
    private def goDown(c: Coordinate, currentSize: Int, currentPosition: Int, toPosition: Int): Coordinate = {
        updateValueMap(c)
        if(currentPosition == toPosition) c
        else if(c.isLeftDownCorner(currentSize)) goRight(c.right, currentSize, currentPosition+1, toPosition)
        else goDown(c.down, currentSize, currentPosition+1, toPosition)
    }
    private def goRight(c: Coordinate, currentSize: Int, currentPosition: Int, toPosition: Int): Coordinate = {
        updateValueMap(c)
        if(currentPosition == toPosition) c
        else if (c.isRightDownCorner(currentSize)) nextSpiral(c.right, currentSize, currentPosition+1, toPosition)
        else goRight(c.right, currentSize, currentPosition+1, toPosition)
    }
    private def goLeft(c: Coordinate, currentSize: Int, currentPosition: Int, toPosition: Int): Coordinate = {
        updateValueMap(c)
        if(currentPosition == toPosition) c
        else if(c.isLeftUpCorner(currentSize)) goDown(c.down, currentSize, currentPosition+1, toPosition)
        else goLeft(c.left, currentSize, currentPosition+1, toPosition)
    }

    private def nextSpiral(c: Coordinate, currentSize: Int, currentPosition: Int, toPosition: Int): Coordinate = {
        updateValueMap(c)
        if(currentPosition == toPosition) c
        else goUp(c.up, currentSize+1, currentPosition+1, toPosition)
    }

    private def enumerateInSpiral(toPosition: Int): Coordinate = {
        m.clear
        m += (Coordinate(0,0) -> Some(1))

        if(toPosition == 1) Coordinate(0,0)
        else goUp(Coordinate(0,0).right, 1, 2, toPosition)
    }

    def fromSpiralCoordinate(position: Int): Coordinate = enumerateInSpiral(position)

}

case class Coordinate(x: Int, y: Int) {
    def mannheimDistanceFromOrigin: Int = math.abs(x) + math.abs(y)

    def isRightDownCorner(size: Int): Boolean = {
        x == size && y == -1 * size
    }

    def isRightUpCorner(size: Int): Boolean = {
        x == size && y == size
    }

    def isLeftUpCorner(size: Int): Boolean = {
        -1 * x == size && y == size
    }

    def isLeftDownCorner(size: Int): Boolean = {
        x == -1 * size && y == -1 * size
    }

    def up: Coordinate = Coordinate(x, y+1)
    def down: Coordinate = Coordinate(x, y-1)
    def left: Coordinate = Coordinate(x-1, y)
    def right: Coordinate= Coordinate(x+1, y)

    def getValue: Int = Coordinate.m(this).get
}
