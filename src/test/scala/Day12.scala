import org.scalatest.FunSuite

class Day12 extends FunSuite {
    private val exampleInput =
        """
          |0 <-> 2
          |1 <-> 1
          |2 <-> 0, 3, 4
          |3 <-> 2, 4
          |4 <-> 2, 3, 6
          |5 <-> 6
          |6 <-> 4, 5
        """.stripMargin

    private val realInput = Main.getStringFromResource("day12")

    test ("first example") {
        assertResult(6)(new PipeGraph(exampleInput).enumerateBFS(0).size)
    }

    test("real input") {
        assertResult(115)(new PipeGraph(realInput).enumerateBFS(0).size)
    }

    test ("count components first example") {
        assertResult(2)(new PipeGraph(exampleInput).countConnectedComponents)
    }

    test("count components real input") {
        assertResult(221)(new PipeGraph(realInput).countConnectedComponents)
    }
}
