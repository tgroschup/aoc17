import org.scalatest.FunSuite

class Day13 extends FunSuite {
    val testInput =
        """|0: 3
          |1: 2
          |4: 4
          |6: 4
          |""".stripMargin

    val realInput = Main.getStringFromResource("day13")
    test("penalty oftestInput") {
        assertResult(24)(new FirewallWalker(testInput).walkThrough)
    }

    test("penalty of real data"){
        assertResult(1588)(new FirewallWalker(realInput).walkThrough)
    }

    test("delayed start on test data") {
        assertResult(0)(new FirewallWalker(testInput).delayStart(10))
    }

    test("min delay on test data") {
        assertResult(10)(new FirewallWalker(testInput).minDelayUndetected)
    }

    test("min delay on real data") {
        assertResult(3865118)(new FirewallWalker(realInput).minDelayUndetected)
    }
}
