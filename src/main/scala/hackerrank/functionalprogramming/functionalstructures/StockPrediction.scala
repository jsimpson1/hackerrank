package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.model.SegmentedTree

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, TreeMap, TreeSet}
import scala.math.Ordering.Implicits.seqDerivedOrdering

object StockPrediction {

  import model._
  import scala.math._

  def main(args: Array[String]): Unit = {

    val inputStr = """5
                     |3 5 2 6 1
                     |2
                     |0 2
                     |2 3""".stripMargin

    val input = Input.parse(inputStr)

    println(s"input=$input")

    val results = calcLengthOfSubArray(input)

    println(results)

  }

  def calcLengthOfSubArray(input: Input): List[Int] =
    input.queries.map { query =>
      calcLengthOfSubArray(input.prices, query)
    }


  def calcLengthOfSubArray(prices: Array[Int], query: Query): Int = {

    val margin = query.margin
    val minimumValue = prices.apply(query.startIndex)
    val maxValue = minimumValue + margin

    def satisfiesConditions(value: Int): Boolean = {
      minimumValue <= value && value <= maxValue
    }

    @tailrec
    def r(index: Int, indexIncrementFn: Int => Int, currentMax: Int, resultMax: Int): Int = {

      if ( index < 0 || index > prices.size - 1) {
        resultMax
      } else {
        val price = prices(index)
        if ( satisfiesConditions(price) ) {
          val nextCurrentMax = currentMax + 1
          val nextMax = if ( nextCurrentMax > resultMax ) nextCurrentMax else resultMax
          r(indexIncrementFn(index), indexIncrementFn, nextCurrentMax, nextMax)
        } else {
          resultMax
        }
      }
    }

    val initialMax = 0

    val right = r(query.startIndex, _ + 1,  initialMax, initialMax)
    val left = r(query.startIndex, _ - 1, initialMax, initialMax)
    val result = left + right - 1
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

    case class Query(
      startIndex: Int,
      margin: Int,
    )
  }

}
