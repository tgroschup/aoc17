class Passphrase(input: String) {
    private val phrases: List[List[String]] = input.split("\n").toList.map(phrase => phrase.split(" ").toList)

    private def checkPhrase(phrase: List[String]): Boolean = {
        for(word <- phrase) {
            var count = 0
            for (other <- phrase) {
                if(word == other) count += 1
                if (count == 2) return false
            }
        }
        true
    }

    def countValid: Int = phrases.foldRight(0)((phrase, result) => if(checkPhrase(phrase)) result + 1 else result)

}
