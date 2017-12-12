import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Pipegram {
    private val pipegrams = mutable.Map[Int, Pipegram]()

    def apply(id: Int): Pipegram = pipegrams.getOrElseUpdate(id, new Pipegram(id))

    def all: Set[Pipegram] = pipegrams.values.toSet

    def clear(): Unit = pipegrams.clear
}

class Pipegram private (val id: Int) {
    private val _pipedTo: mutable.Set[Pipegram] = mutable.Set()

    def pipedTo: Set[Pipegram] = _pipedTo.toSet

    def pipe(other: Pipegram): Unit =  {
        _pipedTo += other
        other._pipedTo += this
    }

    def pipe(other: Int): Unit = pipe(Pipegram(other))
}

object PipeGraphParser extends RegexParsers {
    private def id = "[0-9]+".r ^^ {java.lang.Integer.parseInt}

    private def othersList = id ~ ("," ~> id).* ^^ {case head ~ rest => List(head) ::: rest}

    private def connection = id ~ ("<->" ~> othersList) ^^ {
        case node ~ others =>
            val current= Pipegram(node)
            for(o <- others) {
                current.pipe(o)
            }
    }

    private def file = connection.+

    def apply(in: String): Unit = parse(file, in) match {
        case Success(_, _) =>
        case Failure(msg, _) => throw new Exception("Parsing failed here:\n" + msg)
        case Error(msg, _) => throw new Exception("ERROR! " + msg)
    }

}

class PipeGraph (input: String){
    Pipegram.clear()
    PipeGraphParser(input)

    def enumerateBFS(start: Pipegram): List[Pipegram] = {
        val todo = mutable.Queue[Pipegram](start.pipedTo.toSeq : _ *)
        val done = mutable.Set[Pipegram](start)

        while(todo.nonEmpty) {
            val current = todo.dequeue()

            for(n <- current.pipedTo if !done.contains(n)) {
                todo += n
            }

            done += current
        }

        done.toList
    }

    def enumerateBFS(start: Int): List[Pipegram] = enumerateBFS(Pipegram(start))

    def countConnectedComponents: Int = {
        val visited = mutable.Set[Pipegram]()

        var components = 0

        def dfs(start: Pipegram): Unit = {
            for(n <- start.pipedTo if !visited.contains(n)) {
                visited += n
                dfs(n)
            }
        }

        for(p <- Pipegram.all if !visited.contains(p)) {
            visited += p
            dfs(p)

            components += 1
        }

        components
    }
}
