package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.model.{SegmentedTree, _}

import scala.annotation.tailrec
import scala.language.implicitConversions

object StockPrediction {

  import model._

  def main(args: Array[String]): Unit = {

    val inputStr = """5
                     |3 5 2 6 1
                     |2
                     |0 2
                     |2 3""".stripMargin

    val input = Input.parse(inputStr)

    println(s"input=$input")

    val results = solve(input)

    println(results)

  }

  def solve(input: Input): List[Int] = {
    val blocks = createBlocks(input)
    input.queries.map{ query =>
      solveQuery(query, input.prices, blocks)
    }
  }

  def numberOfBlocks(lengthOfPrices: Int, blockSize: Int): Int =
    if (lengthOfPrices % blockSize != 0)
      lengthOfPrices / blockSize + 1
    else
      lengthOfPrices / blockSize

  def createBlocks(input: Input): Array[Node] = {
    val blockSize = Math.sqrt(input.lengthOfPrices).toInt
    val numOfBlocks = numberOfBlocks(input.lengthOfPrices, blockSize)
    buildBlocks(input.prices, blockSize, numOfBlocks)
  }

  def buildBlocks(values: Array[Int], blockSize: Int, numberOfBlocks: Int): Array[Node] = {

    val lengthOfPrices = values.length

    @tailrec
    def r(current: Int, result: List[Node]): List[Node] = {
      if (current == numberOfBlocks) {
        result
      } else {
        val left = blockSize * current
        val right = Math.min(lengthOfPrices, left + blockSize)
        val minValue =
          (left until right)
            .foldLeft(Int.MaxValue)((cur, idx) =>
              Math.min(cur, values(idx))
            )
        val maxValue =
          (left until right)
            .foldLeft(Int.MinValue)((cur, idx) =>
              Math.max(cur, values(idx))
            )

        r(
          current + 1,
          result.+:(Node(left, right, minValue, maxValue))
        )
      }
    }
    r(0, List[Node]()).reverse.toArray
  }

  def resolveIndex(queryStartIndex: Int, blocks: Array[Node]): Int =  {
    var index = -1
    for (i <- blocks.indices) {
      if ( blocks(i).leftIndex <= queryStartIndex && blocks(i).rightIndex > queryStartIndex ) {
        index = i
      }
    }
    index
  }

  def solveQuery(query: Query, prices: Array[Int], blocks: Array[Node]): Int = {

    val minValue = prices(query.startIndex)
    val maxValue = minValue + query.margin

    @tailrec
    def resolvedBlocksValue(currentIndex: Int, increment: Int, result: Int): Int = {
      if (currentIndex < 0 || currentIndex >= blocks.length) {
        result
      } else if (blocks(currentIndex).minValue >= minValue && blocks(currentIndex).maxValue <= maxValue) {
        resolvedBlocksValue(currentIndex + increment, increment, result + blocks(currentIndex).rightIndex - blocks(currentIndex).leftIndex)
      } else {
        val fi = if (increment == 1) blocks(currentIndex).leftIndex else blocks(currentIndex).rightIndex - 1
        var ret = result
        var i = fi
        while (true) {
          if (prices(i) < minValue || prices(i) > maxValue) {
            return ret
          }
          ret += 1
          i += increment
        }
        -1
      }
    }

    val index = resolveIndex(query.startIndex, blocks)

    val lp: Seq[Int] =
      (blocks(index).leftIndex until query.startIndex)
        .reverse
        .takeWhile(i =>
          prices(i) >= minValue && prices(i) <= maxValue
        )

    val rp =
      (query.startIndex + 1 until blocks(index).rightIndex)
        .takeWhile(i =>
          prices(i) >= minValue && prices(i) <= maxValue
        )

    val leftBlocks =
      if (lp.length == query.startIndex - blocks(index).leftIndex)
        resolvedBlocksValue(index - 1, -1, 0)
      else
        0

    val rightBlocks =
      if (rp.length == blocks(index).rightIndex - query.startIndex - 1)
        resolvedBlocksValue(index + 1, 1, 0)
      else
        0

    val result =  1 + lp.length + rp.length + leftBlocks + rightBlocks

    result
  }

  object model {

    object Input {

      def parse(str: String): Input = {
        val lines =
          str
            .split("\n")

        val numOfQueries = lines(2).toInt

        Input(
          lines(0).toInt,
          lines(1).split(" ").map(_.toInt),
          numOfQueries,
          (0 until numOfQueries)
            .map{ i =>
              val line = lines(i + 3)
              lineToQuery(line)
            }.toList
        )
      }

      def lineToQuery(line: String): Query =
        line
          .split(" ")
          .map(_.toInt) match {
          case Array(startIndex, margin) =>
            Query(startIndex, margin)
        }

    }

    case class Input(
      lengthOfPrices: Int,
      prices: Array[Int],
      numOfQueries: Int,
      queries: List[Query],
    )

    case class Node(
      leftIndex: Int,
      rightIndex: Int,
      minValue: Int,
      maxValue: Int
    )

    case class Query(
      startIndex: Int,
      margin: Int,
    )
  }

}
