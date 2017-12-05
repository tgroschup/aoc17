import org.scalatest.FunSuite

class Day5 extends FunSuite{
    test("example jumplist") {
        val input =
            """|0
               |3
               |0
               |1
               |-3
               |""".stripMargin

        assertResult(5)(new JumpList(input).countJumps)
    }
}
