import org.scalatest.FunSuite

class Day16 extends FunSuite {

    private val testInput = "s1,x3/4,pe/b"
    private val input = Main.getStringFromResource("day16")

    test("do the dance moves on test input") {
        assertResult("baedc")(new ProgramDance(testInput, 'e').dance().mkString)
    }

    test("real result first dance") {
        assertResult("ociedpjbmfnkhlga")(new ProgramDance(input).dance().mkString)
    }

    test("do the dance moves on test input multiple times") {
        assertResult("ceadb")(new ProgramDance(testInput, 'e').dance(2).mkString)
    }

    test("real dance multiple times") {
        assertResult("gnflbkojhicpmead")(new ProgramDance(input).dance(1000000000).mkString)
    }
}
