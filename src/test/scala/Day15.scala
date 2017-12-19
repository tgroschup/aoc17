import org.scalatest.FunSuite

class Day15 extends FunSuite{
    private val fullRunLength = 40000000
    private val fullSecondRun = 5000000

    test ("example generators") {
        assertResult(1)(Judge(5,new GeneratorA(65),new GeneratorB(8921)))
    }

    test ("full run - example generators") {
        assertResult(588)(Judge(fullRunLength,new GeneratorA(65),new GeneratorB(8921)))
    }

    test("count same lower bits real start values") {
        assertResult(619)(Judge(fullRunLength, new GeneratorA(591), new GeneratorB(393)))
    }

    test ("picky example generators") {
        assertResult(1)(Judge(1056,new PickyGenA(65), new PickyGenB(8921)))
    }

    test (" picky full run - example generators") {
        assertResult(309)(Judge(fullSecondRun,new PickyGenA(65), new PickyGenB(8921)))
    }

    test("picky count same lower bits real start values") {
        assertResult(290)(Judge(fullSecondRun, new PickyGenA(591), new PickyGenB(393)))
    }

}
