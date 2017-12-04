object Main extends App {
    def getStringFromResource(name: String): String =
        scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name)).getLines().mkString("\n")

    println("----------------------------")
    println("---- Advent of Code 2017 ---")
    println("----------------------------")
    println("\nDay one:")
    val inputDay1: String = getStringFromResource("day1")
    println("First Solution: " + new CaptchaSolver(inputDay1).sum1)
    println("Second Solution: " + new CaptchaSolver(inputDay1).sum2)

    println("\nDay two:")
    val inputDay2: String = getStringFromResource("day2")
    println("First Solution: " + new Spreadsheet(inputDay2).checksum)
    println("Second Solution: " + new Spreadsheet(inputDay2).sumOfEvenDivides)

    println("\nDay three:")
    val inputDay3: Int = 347991
    println("First Solution: " + Coordinate.fromSpiralCoordinate(inputDay3).mannheimDistanceFromOrigin)
    println("Second Solution: " + Coordinate.m.values.map(_.getOrElse(0)).toList.sorted.find(_ > inputDay3).get)

    println("\nDay four:")
    val inputDay4: String = getStringFromResource("day4")
    println("First Solution: " + new Passphrase(inputDay4).countValid)
    println("Second Solution: " + new Passphrase(inputDay4).countValidWithAnagrams)
}
