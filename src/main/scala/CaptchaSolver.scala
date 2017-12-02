class CaptchaSolver(input: String) {
    val numbers: List[Int] = input.map(c => java.lang.Integer.parseInt(c.toString)).toList

    def sum1: Int = (numbers :+ numbers.head).sliding(2).filter(pair => pair(0) == pair(1)).map(_.head).sum

    def sum2: Int = {
        val rotated = numbers.slice(numbers.length/2, numbers.length) ::: numbers.slice(0, numbers.length/2)
        numbers.zip(rotated).filter(pair => pair._1 == pair._2).map(_._1).sum
    }
}
