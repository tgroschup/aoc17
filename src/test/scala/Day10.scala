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

    test("knot has of AoC 2017") {
        assertResult("33efeb34ea91902bb2f59c9920caa6cd")(new BadHash("AoC 2017").getHash)
    }

    test("knot has of 1,2,3") {
        assertResult("3efbe78a8d82f29979031a4aa0b16a9d")(new BadHash("1,2,3").getHash)
    }

    test("knot has of 1,2,4") {
        assertResult("63960835bcdc130f0b66d7ff4f6a5a8e")(new BadHash("1,2,4").getHash)
    }

}
