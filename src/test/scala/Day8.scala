import org.scalatest.FunSuite

class Day8 extends FunSuite{
    private val realInput = Main.getStringFromResource("day8")

    private val exampleInput = """b inc 5 if a > 1
                         |a inc 1 if b < 5
                         |c dec -10 if a >= 1
                         |c inc -20 if c == 10""".stripMargin


    test("example input - max regfile entry") {
        assertResult(1)(new RegisterInstructionParser(exampleInput).execute.max)
    }

    test("real input - one") {
        assertResult(4416)(new RegisterInstructionParser(realInput).execute.max)
    }

    test("example input - all time max regfile entry") {
        assertResult(10)(new RegisterInstructionParser(exampleInput).execute.allTimeMax)
    }

    test("real input - all time max regfile entry") {
        assertResult(5199)(new RegisterInstructionParser(realInput).execute.allTimeMax)
    }
}
