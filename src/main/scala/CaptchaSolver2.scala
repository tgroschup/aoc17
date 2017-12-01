class CaptchaSolver2(input: String) {
    val numbers: List[Int] = input.map(_.toInt - 48).toList
    val rotated: List[Int] = numbers.slice(numbers.length/2, numbers.length) ::: numbers.slice(0, numbers.length/2)

    /*println(s"lenght: ${numbers.length}")
    println(s"list   : $numbers")
    println(s"rotated: $rotated")*/

    def sum: Int = numbers.zip(rotated).flatMap(pair => if(pair._1 == pair._2) Some(pair._1) else None).sum

}
