import org.scalatest.FunSuite

class Day18 extends FunSuite {

    private val exampleInput =
        """
          |set a 1
          |add a 2
          |mul a a
          |mod a 5
          |snd a
          |set a 0
          |rcv a
          |jgz a -1
          |set a 1
          |jgz a -2
        """.stripMargin

    private val input = Main.getStringFromResource("day18")

    test("example instructions") {
        assertResult(4)(new SoundGenerator(exampleInput).getFirstRecovered)
    }

    test("real run first recovered freq") {
        println(new SoundGenerator(input).getFirstRecovered)
    }

}
