package hackerrank.recursion

object Crosswords101 {

  def main(args: Array[String]): Unit = {
    val input = ""
    //Crossword101Inputs.test0

    val i = parseInput(input)
    println(s"input=${i}")
  }

  case class Coordinate(row: Int, column: Int)

  trait CrosswordChar {
    def value: Char
    def coordinate: Coordinate
  }

  case class Open(
    coordinate: Coordinate
  ) extends CrosswordChar {
    val value: Char = '-'
  }

  case class Closed(
    coordinate: Coordinate
  ) extends CrosswordChar {
    val value: Char = '+'
  }

  case class Value(
    value: Char,
    coordinate: Coordinate
  ) extends CrosswordChar

  case class Word(
    value: String
  )

  case class Row(chars: IndexedSeq[CrosswordChar])

  case class Crossword(rows: List[Row]) {


  }

  case class Input(words: List[Word], crossword: Crossword)

  def parseCrosswordChar(c: Char, row: Int, column: Int): CrosswordChar = c match {
    case '+' => Closed(Coordinate(row, column))
    case '-' => Open(Coordinate(row, column))
    case o => throw new RuntimeException(s"currently do n0t handle case ${o}")
  }

  def parseInput(input: String):Input = {
    val lines = input.lines.toList
    val c = Crossword(
      List()
//      List.range(0, 10).map { row =>
//
//      }
    )
    val words = lines(10).split(";").toList.map(Word)
    Input(words, c)
  }

}
