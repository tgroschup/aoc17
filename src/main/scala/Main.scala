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

}
