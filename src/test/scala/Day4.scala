import org.scalatest.FunSuite

class Day4 extends FunSuite {
    private val input: String = Main.getStringFromResource("day4")
    private val partialInput: String =
        """sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp
            |xmuf znkhaes pggrlp zia znkhaes znkhaes
            |nti rxr bogebb zdwrin
            |sryookh unrudn zrkz jxhrdo gctlyz
            |""".stripMargin

    private val string1 = s"aa bb cc dd ee\n"
    private val string2 = s"aa bb cc dd aa\n"
    private val string3 = s"aa bb cc dd aaa\n"

    test("aa bb cc dd ee") {
        assertResult(1)(new Passphrase(string1).countValid)
    }

    test("aa bb cc dd aa") {
        assertResult(0)(new Passphrase(string2).countValid)
    }

    test("aa bb cc dd aaa") {
        assertResult(1)(new Passphrase(string3).countValid)
    }

    test("Partial input") {
        assertResult(3)(new Passphrase(partialInput).countValid)
    }

    test("Solution one") {
        assertResult(383)(new Passphrase(input).countValid)
    }

    private val string4 = "abcde fghij\n"
    private val string5 = "abcde xyz ecdab\n"
    private val string6 = "a ab abc abd abf abj\n"
    private val string7 = "iiii oiii ooii oooi oooo\n"
    private val string8 = "oiii ioii iioi iiio\n"

    test("abcde fghij") {
        assertResult(1)(new Passphrase(string4).countValidWithAnagrams)
    }

    test("abcde xyz ecdab") {
        assertResult(0)(new Passphrase(string5).countValidWithAnagrams)
    }

    test("a ab abc abd abf abj") {
        assertResult(1)(new Passphrase(string6).countValidWithAnagrams)
    }

    test("iiii oiii ooii oooi oooo") {
        assertResult(1)(new Passphrase(string7).countValidWithAnagrams)
    }

    test("oiii ioii iioi iiio") {
        assertResult(0)(new Passphrase(string8).countValidWithAnagrams)
    }

    test("Solution two") {
        assertResult(265)(new Passphrase(input).countValidWithAnagrams)
    }
}
