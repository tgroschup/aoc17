class Spreadsheet(input: String) {
    val rowsOfInput: List[List[String]] = input.split("\n").toList.map(row => row.split("\\s+").toList)
    val rows: List[List[Int]] = rowsOfInput.map(row => row.map(entry => java.lang.Integer.parseInt(entry)))

    def checksum: Int = rows.map(r => r.max - r.min).sum

    def sumOfEvenDivides: Int = {
        val quotients = for(row <- rows)
            yield for(entry <- row; other <- row if other > entry && other % entry == 0)
                yield other/entry
        quotients.flatten.sum
    }
}