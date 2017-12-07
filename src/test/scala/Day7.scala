import org.scalatest.FunSuite

class Day7 extends FunSuite {
    private val exampleIntput =
        """
          |pbga (66)
          |xhth (57)
          |ebii (61)
          |havc (66)
          |ktlj (57)
          |fwft (72) -> ktlj, cntj, xhth
          |qoyq (66)
          |padx (45) -> pbga, havc, qoyq
          |tknk (41) -> ugml, padx, fwft
          |jptl (61)
          |ugml (68) -> gyxo, ebii, jptl
          |gyxo (61)
          |cntj (57)
          |""".stripMargin

    test("example input") {
        assertResult("tknk")(new ProgramTower(exampleIntput))
    }

    test("example find unbalanced program") {
        assertResult(60)(new ProgramTower(exampleIntput).findUnbalancedDisk)
    }
}
