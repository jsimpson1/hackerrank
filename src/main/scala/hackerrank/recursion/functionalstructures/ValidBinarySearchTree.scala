package hackerrank.recursion.functionalstructures


object ValidBinarySearchTree {

  import model._

  def main(args: Array[String]): Unit = {

    val input = """5
                  |3
                  |1 2 3
                  |3
                  |2 1 3
                  |6
                  |3 2 1 5 4 6
                  |4
                  |1 3 4 2
                  |5
                  |3 4 5 1 2""".stripMargin

    parseInput(input).foreach { i =>
      val result = isValidBst(i.values)
      println(s"case $i => $result")
    }

  }

  def parseInput(str: String): List[Input] = {
    str
      .split("\n")
      .toList
      .tail
      .grouped(2)
      .map(v =>
        Input(
          v.head.toInt,
          v.tail.head.split(" ").map(_.toInt).toList
        )
      ).toList
  }

  def isValidBst(v: List[Int]): Boolean = {
    var index: Int = 0
    def r(min: Int, max: Int): Unit = {
      if ( index <= v.size - 1) {
        val node = v.apply(index)
        if ( min <= node && node <= max) {
          index += 1
          r(min, node)
          r(node, max)
        }
      }
    }
    r(Int.MinValue, Int.MaxValue)
    index == v.size
  }


  object model {

    case class Input(numOfElem: Int, values: List[Int])

  }



}
