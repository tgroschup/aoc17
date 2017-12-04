import org.scalatest.FunSuite

class Day2 extends FunSuite{
    private val testCase = Main.getStringFromResource("day2")

    test("test matrix") {
        val input: String =
            """|5    1 9   5
               |7    5   3
               |2    4   6   8
               |""".stripMargin

        assertResult(18)(new Spreadsheet(input).checksum)
    }

    test("spredsheet day two frist part") {
        assertResult(44216)(new Spreadsheet(testCase).checksum)
    }

    test("Quotient sum"){
        val input: String =
            """|5 9 2 8
               |9 4 7 3
               |3 8 6 5
               |""".stripMargin

        assertResult(9)(new Spreadsheet(input).sumOfEvenDivides)
    }

    test("calc quotient sum") {
        assertResult(320)(new Spreadsheet(testCase).sumOfEvenDivides)
    }
}
