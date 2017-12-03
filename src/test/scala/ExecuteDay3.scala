import org.scalatest.FunSuite

class ExecuteDay3 extends FunSuite{
    test("origin") {
        assertResult(0)(Coordinate.fromSpiralCoordinate(1).mannheimDistanceFromOrigin)
    }

    test("12") {
        assertResult(3)(Coordinate.fromSpiralCoordinate(12).mannheimDistanceFromOrigin)
    }

    test("23") {
        assertResult(2)(Coordinate.fromSpiralCoordinate(23).mannheimDistanceFromOrigin)
    }

    test("1024") {
        assertResult(31)(Coordinate.fromSpiralCoordinate(1024).mannheimDistanceFromOrigin)
    }

    test("347991") {
        println("First solution for day three is: " + Coordinate.fromSpiralCoordinate(347991).mannheimDistanceFromOrigin)
    }
}
