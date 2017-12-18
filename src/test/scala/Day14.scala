import org.scalatest.FunSuite

class Day14 extends FunSuite{

    private val exampleInput = "flqrgnkx"

    private val realInput = "hxtvlmkl"

    test("part one example") {
        assertResult(8108)(new Defragmenter(exampleInput).count)
    }

    test("real input") {
        assertResult(8214)(new Defragmenter(realInput).count)
    }

    test("real input connected components") {
        assertResult(8214)(new Defragmenter(realInput).countConnectedComponents)
    }
}
