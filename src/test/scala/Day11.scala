import org.scalatest.FunSuite

class Day11 extends FunSuite{
    private val realInput = Main.getStringFromResource("day11")

    test("3x north") {
        assertResult(3)(new HexagonInfinity("n,n,n").distanceAfterWalk)
    }

    test("3x north-east") {
        assertResult(3)(new HexagonInfinity("ne,ne,ne").distanceAfterWalk)
    }

    test("there and back again") {
        assertResult(0)(new HexagonInfinity("ne,ne,sw,sw").distanceAfterWalk)
    }

    test("2x ne, 2x s") {
        assertResult(2)(new HexagonInfinity("ne,ne,s,s").distanceAfterWalk)
    }

    test("bit south east and south west") {
        assertResult(3)(new HexagonInfinity("se,sw,se,sw,sw").distanceAfterWalk)
    }

    test("distance in real input") (
        assertResult(720)(new HexagonInfinity(realInput).distanceAfterWalk)
    )

    test("furthes distance - there and back again") {
        assertResult(2)(new HexagonInfinity("ne,ne,sw,sw").furthestDistance)
    }

    test("furthes distance in real input") (
        assertResult(1485)(new HexagonInfinity(realInput).furthestDistance)
    )
}
