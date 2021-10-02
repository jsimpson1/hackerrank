package hackerrank.functionalprogramming.recursion.geometry

object ConcavePolygon {

  def main(args: Array[String]): Unit = {

    val inputStr = ConcaveInputs.test0

    printIsConcave(inputToPoints(inputStr))
  }

  def inputToPoints(input: String): Set[Point] = {
    val inputLines = input.split("\\n").toList
    val numOfPoints = inputLines.head.toInt
    List
      .range(0, numOfPoints)
      .map { i =>
        val r = inputLines.tail(i).split(" ")
        Point(r(0).toDouble, r(1).toDouble)
      }
      .toSet
  }

  def printIsConcave(points: Set[Point]): Unit = {
    if ( Polygon(points).isConcave )
      println("YES")
    else
      println("NO")
  }

}
