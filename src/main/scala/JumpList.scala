class JumpList(jumpList: String) {
    val jumps : List[Int]= jumpList.split("\n").map(java.lang.Integer.parseInt).toList

    private def jump(jumps: List[Int], i: Int, modifyIndex: Int => Int, jumpCount: Int = 0): Int =
        if(i >= jumps.size || i < 0) jumpCount
        else if(jumpCount < 0 ) {
            println(s"More jumps than INT can hold, wtf")
            System.exit(-1)
            0
        }
        else jump(jumps.updated(i, modifyIndex(jumps(i))), i + jumps(i), modifyIndex, jumpCount +1)

    def countJumps: Int = jump(jumps, 0, i => i + 1)
    def countJumpsInAnAbsurdWay: Int = jump(jumps, 0, i => if(i >= 3) i-1 else i + 1)
}
