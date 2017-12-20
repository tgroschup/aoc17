import scala.util.{Failure, Success, Try}

object Main extends App {
    def getStringFromResource(name: String): String =
        scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name)).getLines().mkString("\n")

    private def number2Word(i: Int): String = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five"
                                            , 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten"
                                            , 11 -> "eleven", 12 -> "twelve").getOrElse(i, i.toString)

    private def dayTemplate(day: Int, solution1: String => Any = _ => "TODO"
                                    , solution2: String => Any = _ => "TODO"): Unit = {
        println(s"\nDay ${number2Word(day)}:")
        val input: String = Try(getStringFromResource(s"day$day")) match {
            case Success(s) => s
            case Failure(_) => ""
        }
        println("First Solution: " + solution1(input))
        println("Second Solution: " + solution2(input))
    }


    println("----------------------------")
    println("---- Advent of Code 2017 ---")
    println("----------------------------")

    dayTemplate(1, new CaptchaSolver(_).sum1, new CaptchaSolver(_).sum2)

    dayTemplate(2, new Spreadsheet(_).checksum, new Spreadsheet(_).sumOfEvenDivides)

    val inputDay3: Int = 347991
    dayTemplate(3, _ => Coordinate.fromSpiralCoordinate(inputDay3).mannheimDistanceFromOrigin
                , _ => Coordinate.m.values.map(_.getOrElse(0)).toList.sorted.find(_ > inputDay3).get)

    dayTemplate(4, new Passphrase(_).countValid, new Passphrase(_).countValidWithAnagrams)

    dayTemplate(5, new JumpList(_).countJumps, new JumpList(_).countJumpsInAnAbsurdWay)

    dayTemplate(6, new Reallocator(_).countRedistributions, new Reallocator(_).getLoopSize)

    dayTemplate(7, new ProgramTower(_).root.name, new ProgramTower(_).findUnbalancedDisk)

    dayTemplate(8, new RegisterInstructionParser(_).execute.max, new RegisterInstructionParser(_).execute.allTimeMax)

    dayTemplate(9, new CharacterStream(_).score, new CharacterStream(_).garbageCount)

    dayTemplate(10, new BadHash(_).transform(256), new BadHash(_).getHash)

    dayTemplate(11, new HexagonInfinity(_).distanceAfterWalk, new HexagonInfinity(_).furthestDistance)

    dayTemplate(12, new PipeGraph(_).enumerateBFS(0).size, new PipeGraph(_).countConnectedComponents)

    dayTemplate(13, new FirewallWalker(_).walkThrough, new FirewallWalker(_).minDelayUndetected)

    dayTemplate(14, new Defragmenter(_).count, new Defragmenter(_).countConnectedComponents)

    dayTemplate(15, _ => Judge(4000000, new GeneratorA(65), new GeneratorB(8921))
                    , _ => Judge(500000, new PickyGenA(65), new PickyGenB(8921)))

    dayTemplate(16, new ProgramDance(_).dance().mkString, new ProgramDance(_).dance(1000000000).mkString)

    dayTemplate(17, _ => new Stormlock(304).runBuffer(2017),_ => new Stormlock(304).secondValueAfter(50000000))
}
