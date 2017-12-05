class JumpList(jumpList: String) {
    val jumps : Array[Int]= jumpList.split("\n").map(java.lang.Integer.parseInt)

    private def jump(jumps: Array[Int], i: Int, modifyIndex: Int => Int, jumpCounter: Int = 0): Int =
        if(i >= jumps.length || i < 0) jumpCounter
        else {
            val newIndex = i + jumps(i)
            jumps(i) = modifyIndex(jumps(i))
            jump(jumps, newIndex, modifyIndex, jumpCounter +1)
        }

    /*private def jumpIterative(jumps: Array[Int], modifyIndex: Int => Int): Int = {
        var jumpCounter = 0
        var nextIndex = 0

        while(nextIndex >= 0 && nextIndex < jumps.length) {
            val current = nextIndex
            nextIndex = nextIndex + jumps(current)
            jumps(current) = modifyIndex(jumps(current))
            jumpCounter += 1
        }

        jumpCounter
    }*/

    def countJumps: Int = jump(jumps, 0, i => i + 1)
    //def countJumpsIterative: Int = jumpIterative(jumps, i => i + 1)
    def countJumpsInAnAbsurdWay: Int = jump(jumps, 0, i => if(i >= 3) i-1 else i + 1)
    //def countJumpsInAnAbsurdWayIterative: Int = jumpIterative(jumps, i => if(i >= 3) i-1 else i + 1)
}
