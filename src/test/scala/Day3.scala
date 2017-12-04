import org.scalatest.FunSuite

class Day3 extends FunSuite{
    val input = 347991

    test("origin") {
        assertResult(0)(Coordinate.fromSpiralCoordinate(1).mannheimDistanceFromOrigin)
    }

    test("first circle") {
        assertResult(Coordinate(1,0))(Coordinate.fromSpiralCoordinate(2)) //go up
        assertResult(Coordinate(1,1))(Coordinate.fromSpiralCoordinate(3)) //go left
        assertResult(Coordinate(0,1))(Coordinate.fromSpiralCoordinate(4)) //go left
        assertResult(Coordinate(-1,1))(Coordinate.fromSpiralCoordinate(5)) //go down
        assertResult(Coordinate(-1,0))(Coordinate.fromSpiralCoordinate(6)) //go down
        assertResult(Coordinate(-1,-1))(Coordinate.fromSpiralCoordinate(7))
        assertResult(Coordinate(0,-1))(Coordinate.fromSpiralCoordinate(8))
        assertResult(Coordinate(1,-1))(Coordinate.fromSpiralCoordinate(9))
    }

    test("transition to and first step in second circle") {
        assertResult(Coordinate(2,-1))(Coordinate.fromSpiralCoordinate(10))
        assertResult(Coordinate(2,0))(Coordinate.fromSpiralCoordinate(11))
    }

    test("critical corners") {
        assertResult(Coordinate(2,2))(Coordinate.fromSpiralCoordinate(13))
        assertResult(Coordinate(1,2))(Coordinate.fromSpiralCoordinate(14))
        assertResult(Coordinate(-2,2))(Coordinate.fromSpiralCoordinate(17))
    }

    test("12") {
        assertResult(Coordinate(2,1))(Coordinate.fromSpiralCoordinate(12))
        assertResult(3)(Coordinate.fromSpiralCoordinate(12).mannheimDistanceFromOrigin)
    }

    test("23") {
        assertResult(2)(Coordinate.fromSpiralCoordinate(23).mannheimDistanceFromOrigin)
    }

    test("1024") {
        assertResult(31)(Coordinate.fromSpiralCoordinate(1024).mannheimDistanceFromOrigin)
    }

    test(input.toString) {
        assertResult(480)(Coordinate.fromSpiralCoordinate(input).mannheimDistanceFromOrigin)
    }

    test("get value at origin") {
        assertResult(1)(Coordinate.fromSpiralCoordinate(1).getValue)
    }

    test("values in first circle") {
        assertResult(1)(Coordinate.fromSpiralCoordinate(2).getValue)
        assertResult(2)(Coordinate.fromSpiralCoordinate(3).getValue)
        assertResult(4)(Coordinate.fromSpiralCoordinate(4).getValue)
        assertResult(5)(Coordinate.fromSpiralCoordinate(5).getValue)
        assertResult(10)(Coordinate.fromSpiralCoordinate(6).getValue)
        assertResult(11)(Coordinate.fromSpiralCoordinate(7).getValue)
        assertResult(23)(Coordinate.fromSpiralCoordinate(8).getValue)
        assertResult(25)(Coordinate.fromSpiralCoordinate(9).getValue)
        assertResult(26)(Coordinate.fromSpiralCoordinate(10).getValue)

    }

    test(s"get first bigger value than $input") {
        Coordinate.fromSpiralCoordinate(input) // Build coordinate map
        assertResult(349975)(Coordinate.m.values.map(_.getOrElse(0)).toList.sorted.find(_ > input).get)
    }
}
