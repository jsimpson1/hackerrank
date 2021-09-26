package hackerrank.recursion.functionalstructures

import hackerrank.recursion.functionalstructures.model.SegmentedTree

object RangeMinimumQuery {

  import model._

  def main(args: Array[String]): Unit = {

    val inputStr = """10 5
                     |10 20 30 40 11 22 33 44 15 5
                     |0 5
                     |1 2
                     |8 9
                     |0 9
                     |4 6""".stripMargin

    val input = parseInput(inputStr)

    println(s"input=$input")

    val solutions = calcSolutions(input)

    println(s"solutions:${solutions.mkString("\n", "\n", "")}")
  }

  def parseInput(str: String): Input = {
    val lines = str.split("\n")

    val mn: Array[String] = lines.head.split(" ")

    val m = mn(0).toInt
    val n = mn(1).toInt
    val arr: List[Int] = lines(1).split(" ").map(_.toInt).toList

    val queries =
      (2 to n + 1)
        .map{ i =>
          val lr = lines(i).split(" ").map(_.toInt)
          QueryParm(lr(0), lr(1))
        }.toList

    Input(m, n, arr, queries)
  }

  implicit def min(x: Int, y: Int): Int =
    scala.math.min(x, y)


  def calcSolutions(input: Input): List[Int] = {

    lazy val segmentedTree =
      SegmentedTree
        .build[Int](input.arr, 0)

    input
      .queries
      .map( query =>
        SegmentedTree
          .query(segmentedTree, query.leftIndex, query.rightIndex)
      )
  }

  object model {

    case class QueryParm(
      leftIndex: Int,
      rightIndex: Int
    )

    case class Input(
      m: Int,
      n: Int,
      arr: List[Int],
      queries: List[QueryParm]
    )

  }

}
