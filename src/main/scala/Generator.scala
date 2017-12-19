abstract class Generator(val startValue: Int) {
    protected val factor: Int
    private val divisor = 2147483647
    private var prev: Long = startValue

    protected def nextInternal: Long = {
        val next = (prev * factor) % divisor
        prev = next
        next
    }

    def next: Long = nextInternal
}

class GeneratorA(startValue: Int) extends Generator(startValue) {
    val factor = 16807
}

class GeneratorB(startValue: Int) extends Generator(startValue) {
    val factor = 48271
}

sealed trait PickyValueSelection { this: Generator =>
    protected val pickySelector: Int
    override def next: Long = {
        var next = nextInternal
        while(next % pickySelector != 0) {
            next = nextInternal
        }
        next
    }
}

class PickyGenA(startValue: Int) extends GeneratorA(startValue) with PickyValueSelection {
    override val pickySelector: Int = 4
}

class PickyGenB(startValue: Int) extends GeneratorB(startValue) with PickyValueSelection {
    override val pickySelector: Int = 8
}

object Judge {
    def apply(count: Int, a: Generator, b: Generator): Int = {
        def check(checksToDo: Int, found: Int = 0): Int = {
            if(checksToDo == 0) found
            else {
                if((a.next & 0xFFFF) == (b.next & 0xFFFF))
                    check(checksToDo -1, found +1)
                else
                    check(checksToDo -1 , found)
            }
        }
        check(count)
    }
}
