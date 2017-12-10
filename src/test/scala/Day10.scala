import org.scalatest.FunSuite

class Day10 extends FunSuite {
    val input = Main.getStringFromResource("day10")


    test("example list") {
        assertResult(12)(new BadHash("3,4,1,5").transform(5))
    }

    test("example input") {
        assertResult(13760)(new BadHash(input).transform(256))
    }

    test("knot has of empty string") {
        assertResult("a2582a3a0e66e6e86e3812dcb672a272")(new BadHash("").getHash)
    }

}
