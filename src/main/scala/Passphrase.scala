class Passphrase(input: String) {
    private val phrases: List[List[String]] = input.split("\n").toList.map(phrase => phrase.split(" ").toList)

    private def checkPhrase(phrase: List[String], isInvalid: (String, String) => Boolean): Boolean = {
        for(word <- phrase) {
            var errorCount = 0
            for (other <- phrase) {
                if(isInvalid(word, other)) errorCount += 1
                if (errorCount == 2) return false
            }
        }
        true
    }

    private def isAnagram(s1: String, s2: String): Boolean = s1.sorted == s2.sorted

    private def validCounter(phrases: List[List[String]], detectValidString: (String, String) => Boolean): Int =
        phrases.foldRight(0)((phrase, result) => if(checkPhrase(phrase, detectValidString)) result + 1 else result)

    def countValid: Int = validCounter(phrases, _ == _)
    def countValidWithAnagrams: Int = validCounter(phrases, isAnagram)

}

