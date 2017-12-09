import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object ProgramInTower {
    private val programs = mutable.Map[String, ProgramInTower]()

    def apply(name: String): ProgramInTower = programs(name)
    def apply(name: String, weight: Int, children: List[ProgramInTower]): ProgramInTower =
        programs.getOrElseUpdate(name, new ProgramInTower(name, weight, children))

    def clear: Unit = programs.clear
}
case class ProgramInTower(name: String, weight: Int, children: List[ProgramInTower]) {
    override def toString: String = name + "(" + weight.toString + ")"
}

object ProgramTowerParser extends RegexParsers {
    private val weights = mutable.Map[String, Int]()
    private val children = mutable.Map[String, List[String]]()

    private def name = "[a-z]+".r
    private def weight = "(" ~> "[0-9]+".r  <~ ")" ^^ {java.lang.Integer.parseInt}
    private def childList = "->" ~> name ~ (", " ~> name).* ^^ {case head ~ rest => List(head) ::: rest}
    private def programDeclaration = name ~ weight ~ childList.? ^^ {
        case name ~ weight ~ childList =>
            weights(name) = weight
            children(name) = childList.getOrElse(List())
    }

    private def programTower = programDeclaration.+

    private def buildTree: ProgramInTower = {
        val leaves: Set[String] = children.filter(_._2.isEmpty).keys.toSet
        val done: mutable.MutableList[String] = mutable.MutableList(leaves).flatten
        
        leaves.foreach(l => ProgramInTower(l, weights(l), List()))

        val todo: mutable.Set[String] = mutable.Set(children.keys.toSet -- leaves).flatten

        while(todo.nonEmpty) {
            val currentLevel: Set[String] = todo.filter(n => children(n).foldRight(true){
                case (child, result) => done.contains(child) && result
            }).toSet

            for(node <- currentLevel) {
                val nodesChildren: List[ProgramInTower] = children(node).map(ProgramInTower(_))
                val weight = weights(node)
                ProgramInTower(node, weight, nodesChildren)
            }

            todo --= currentLevel
            done ++= currentLevel
        }
        ProgramInTower(done.last)
    }

    def apply(input: String): ProgramInTower = {
        ProgramInTower.clear
        weights.clear()
        children.clear()
        parse(programTower, input) match {
            case Success(_, _) => buildTree
            case Failure(msg, _)  => throw new Exception("Parsing failed here:\n" + msg)
            case Error(msg, _) => throw new Exception("ERROR! " + msg)
        }
    }
}

class ProgramTower(input: String) {
    case class DiskUnbalanced(node: ProgramInTower, newWeight: Int) extends Exception(s"Unablanced in disk $node, new weight should be $newWeight")

    val root: ProgramInTower = ProgramTowerParser(input)

    def getRootNodeName: String = root.name

    def findUnbalancedDisk: Int = {
        def calculateWeigh(node: ProgramInTower, level: Int = 0): Int = {
            val subweight: Int = if(node.children.nonEmpty) {
                val childrenWithSubweights = node.children.map(c => c -> calculateWeigh(c, level + 1)).toMap
                val subweights = childrenWithSubweights.values
                val countedSubweights = subweights.groupBy(identity).mapValues(_.size)

                if(countedSubweights.size == 2) {
                    val oddOne = countedSubweights.minBy(_._2)._1
                    val others = countedSubweights.maxBy(_._2)._1

                    val oddChild: ProgramInTower = childrenWithSubweights.find(_._2 == oddOne).get._1
                    val diff = math.abs(oddOne - others)

                    throw DiskUnbalanced(oddChild, oddChild.weight - diff)
                }

                subweights.sum
            } else {
                0
            }
            node.weight + subweight
        }

        try calculateWeigh(root)
        catch {
            case DiskUnbalanced(_, correctedWeight) => correctedWeight
        }
    }
}
