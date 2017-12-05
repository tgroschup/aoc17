import org.scalatest.FunSuite

class Day5 extends FunSuite{
    private val input =
        """|0
           |3
           |0
           |1
           |-3
           |""".stripMargin

    test("example jumplist") {
        assertResult(5)(new JumpList(input).countJumps)
    }

    test("example jumplist - iter") {
        assertResult(5)(new JumpList(input).countJumpsIterative)
    }

    test("real example") {
        val input = Main.getStringFromResource("day5")
        assertResult(372139)(new JumpList(input).countJumps)
    }

    test("real example - iter") {
        val input = Main.getStringFromResource("day5")
        assertResult(372139)(new JumpList(input).countJumpsIterative)
    }

    test("example jumplist counted strange") {
        assertResult(10)(new JumpList(input).countJumpsInAnAbsurdWay)
    }

    test("example jumplist counted strange but iterative") {
        assertResult(10)(new JumpList(input).countJumpsInAnAbsurdWayIterative)
    }

    test("real example absurd jumps") {
        val input = Main.getStringFromResource("day5")
        assertResult(29629538)(new JumpList(input).countJumpsInAnAbsurdWay)
    }

    test("real example absurd jumps but iterative") {
        val input = Main.getStringFromResource("day5")
        assertResult(29629538)(new JumpList(input).countJumpsInAnAbsurdWayIterative)
    }
}
