import org.scalatest.FunSuite

class Day6 extends FunSuite{
    private val input = "0 2 7 0"
    private val file = Main.getStringFromResource("day6")

    test("example input") {
        assertResult(5)(new Reallocator(input).countRedistributions)
    }

    test("real input") {
        assertResult(12841)(new Reallocator(file).countRedistributions)
    }

    test("count loop size") {
        val r = new Reallocator(input)

        r.countRedistributions
        assertResult(4)(r.getLoopSize)
    }

    test("count loop size for real input") {
        val r = new Reallocator(file)

        r.countRedistributions
        assertResult(8038)(r.getLoopSize)
    }
}
