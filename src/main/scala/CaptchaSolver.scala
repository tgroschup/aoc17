class CaptchaSolver(input: String) {
    val numbers: List[Int] = input.map(_.toInt - 48).toList :+ input(0).toInt - 48

    def sum: Int = numbers.sliding(2).filter(pair => pair(0) == pair(1)).map(_.head).sum
}
