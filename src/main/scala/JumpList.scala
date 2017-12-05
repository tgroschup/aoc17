class JumpList(jumpList: String) {
    val jumps : List[Int]= jumpList.split("\n").map(java.lang.Integer.parseInt).toList

    private def jump(jumps: List[Int], i: Int, jumpCount: Int = 0): Int =
        if(i >= jumps.size || i < 0) jumpCount
        else jump(jumps.updated(i, jumps(i) + 1), i + jumps(i), jumpCount +1)

    def countJumps: Int = jump(jumps, 0)
}
