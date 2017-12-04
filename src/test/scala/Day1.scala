import org.scalatest.FunSuite

class Day1 extends FunSuite {
    private val input = Main.getStringFromResource("day1")

    test("1122") {
        assertResult(3)(new CaptchaSolver("1122").sum1)
    }

    test("1111") {
        assertResult(4)(new CaptchaSolver("1111").sum1)
    }

    test("1234") {
        assertResult(0)(new CaptchaSolver("1234").sum1)
    }

    test("91212129") {
        assertResult(9)(new CaptchaSolver("91212129").sum1)
    }

    test("first result") {
        assertResult(1203)(new CaptchaSolver(input).sum1)
    }

    test("1212") {
        assertResult(6)(new CaptchaSolver("1212").sum2)
    }

    test("1221") {
        assertResult(0)(new CaptchaSolver("1221").sum2)
    }

    test("123425") {
        assertResult(4)(new CaptchaSolver("123425").sum2)
    }

    test("123123") {
        assertResult(12)(new CaptchaSolver("123123").sum2)
    }

    test("12131415") {
        assertResult(4)(new CaptchaSolver("12131415").sum2)
    }

    test("second result") {
        assertResult(1146)(new CaptchaSolver(input).sum2)
    }
}
