import org.scalatest.FunSuite

class Day17 extends FunSuite{
    test("small spinlock doing its thing") {
        val s = new Stormlock(3)

        val res = s.runBuffer(9)

        assertResult(List(0,9,5,7,2,4,3,8,6,1))(s.currentBuffer)
        assertResult(5)(res)
    }

    test("example first problem long test run") {
        val s = new Stormlock(3)
        assertResult(638)(s.runBuffer(2017))
    }

    test("real data") {
        assertResult(1173)(new Stormlock(304).runBuffer(2017))
    }

    test("real data second part") {
        val s = new Stormlock(304)

        val res = s.runBuffer(50000)//(50000000)

        println(s"start of buffer ${s.currentBuffer.slice(0, 10)}")
    }

    test("calculate value at 2nd place without inserting") {
        assertResult(9)(new Stormlock(3).secondValueAfter(9))
    }

    test("real input first day value at 2nd") {
        val s = new Stormlock(304)
        s.runBuffer(2017)
        val expectedResult = s.currentBuffer(1)
        assertResult(expectedResult)(new Stormlock(304).secondValueAfter(2017))
    }

    test("real second star") {
        println(new Stormlock(304).secondValueAfter(50000000))
    }
}
