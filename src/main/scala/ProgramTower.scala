import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object ProgramInTower {
    private val programs = mutable.Map[String, ProgramInTower]()

    def apply(name: String): ProgramInTower = programs(name)
    def apply(name: String, weight: Int, children: List[ProgramInTower]): ProgramInTower =
        programs.getOrElseUpdate(name, new ProgramInTower(name, weight, children))
}
case class ProgramInTower(name: String, weight: Int, children: List[ProgramInTower]) {
    override def toString = name + "(" + weight.toString + ")"
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

    def apply(input: String): ProgramInTower = { //should return root of tree
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
                val s0 = calculateWeigh(node.children(0), level + 1)
                val s1 = calculateWeigh(node.children(1), level + 1)
                val s2 = calculateWeigh(node.children(2), level + 1)

                val subweight = s0 + s1 + s2

                println(s"node: $node")
                if(node.children.nonEmpty) {
                    println(s"children ($subweight): ${node.children(0)}($s0), ${node.children(1)}($s1) ${node.children(2)}($s2)")
                }

                if (s0 == s1 && s2 != s0) {
                    val diff = math.abs(s0-s2)
                    val child = node.children(2)
                    throw DiskUnbalanced(child, child.weight - diff)
                } else if (s0 == s2 && s0 != s1 ) {
                    val diff = math.abs(s0-s1)
                    val child = node.children(1)
                    throw DiskUnbalanced(child, child.weight - diff)
                } else if (s1 == s2 && s1 != s0) {
                    val diff = math.abs(s0-s1)
                    val child = node.children(0)
                    throw DiskUnbalanced(child, child.weight - diff)
                }

                subweight
            } else {
                0
            }
            node.weight + subweight
        }

        try calculateWeigh(root)
        catch {
            case DiskUnbalanced(node, weight) => weight
        }
    }
}
