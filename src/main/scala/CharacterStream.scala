class CharacterStream (val stream: IndexedSeq[Char]) {
    private case class Result(score: Int, garbageCount: Int)

    private def consume(s: IndexedSeq[Char], pos: Int, inGarbage: Boolean, currentScore: Int, totalScore: Int, garbageCount: Int): Result =
        if(pos < s.length) {
            s(pos) match {
                case '!' => consume(s, pos + 2, inGarbage, currentScore, totalScore, garbageCount)
                case '{' => if (inGarbage) {
                    consume(s, pos + 1, inGarbage, currentScore, totalScore, garbageCount+1)
                } else {
                    consume(s, pos + 1, inGarbage, currentScore + 1, totalScore, garbageCount)
                }
                case '}' => if (inGarbage) {
                    consume(s, pos + 1, inGarbage, currentScore, totalScore, garbageCount+1)
                } else {
                    consume(s, pos + 1, inGarbage, currentScore - 1, totalScore + currentScore, garbageCount)
                }
                case '<' => consume(s, pos + 1, inGarbage = true, currentScore, totalScore, if(inGarbage) {
                    garbageCount + 1
                } else {
                    garbageCount
                })
                case '>' => consume(s, pos + 1, inGarbage = false, currentScore, totalScore, garbageCount)
                case _ => consume(s, pos + 1, inGarbage, currentScore, totalScore, if(inGarbage) {
                        garbageCount + 1
                    } else {
                        garbageCount
                    })
            }
        } else {
            Result(totalScore, garbageCount)
        }

    def score: Int = consume(stream, 0, inGarbage = false, 0, 0, 0).score
    def garbageCount: Int = consume(stream, 0, inGarbage = false, 0,0,0).garbageCount
}
