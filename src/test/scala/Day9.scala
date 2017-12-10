import org.scalatest.FunSuite

class Day9 extends FunSuite {

    val input = Main.getStringFromResource("day9")

    test("one group") {
        assertResult(1)(new CharacterStream("{}").score)
    }

    test("three groups nested") {
        assertResult(6)(new CharacterStream("{{{}}}").score)
    }

    test("three groups, two parallel") {
        assertResult(5)(new CharacterStream("{{},{}}").score)
    }

    test("even more complex groups") {
        assertResult(16)(new CharacterStream("{{{},{},{{}}}}").score)
    }

    test("one group with garbage") {
        assertResult(1)(new CharacterStream("{<a>,<a>,<a>,<a>}").score)
    }

    test("five groups with garbage") {
        assertResult(9)(new CharacterStream("{{<ab>},{<ab>},{<ab>},{<ab>}}").score)
    }

    test("double cancelation") {
        assertResult(9)(new CharacterStream("{{<!!>},{<!!>},{<!!>},{<!!>}}").score)
    }

    test("with garbage and cancellation") {
        assertResult(3)(new CharacterStream("{{<a!>},{<a!>},{<a!>},{<ab>}}").score)
    }

    test("total score with real input") {
        assertResult(10050)(new CharacterStream(input).score)
    }

    test("count empty garbage") {
        assertResult(0)(new CharacterStream("<>").garbageCount)
    }

    test("count random chars in garbage") {
        assertResult(17)(new CharacterStream("<random characters>").garbageCount)
    }

    test("garbage start characters in garbage") {
        assertResult(3)(new CharacterStream("<<<<>").garbageCount)
    }

    test("garbage and cancellation") {
        assertResult(2)(new CharacterStream("<{!>}>").garbageCount)
    }

    test("garbage and double cancellation") {
        assertResult(0)(new CharacterStream("<!!>}>").garbageCount)
    }

    test("garbage and more cancellation") {
        assertResult(0)(new CharacterStream("<!!!>>").garbageCount)
    }

    test("real garbage and more cancellation") {
        assertResult(10)(new CharacterStream(""""<{o"i!a,<{i<a>""").garbageCount)
    }

    test("real input count garbage") {
        assertResult(4482)(new CharacterStream(input).garbageCount)
    }
}
