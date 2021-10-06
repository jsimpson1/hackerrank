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


//  def calcLengthOfSubArray(originalPrices: Array[Int], query: Query): Int = {
//
//    val margin = query.margin
//    val minValue = originalPrices.apply(query.startIndex)
//    val maxValue = minValue + margin
//
//    def satisfiesConditions(value: Int): Boolean = {
//      minValue <= value && value <= maxValue
//    }
//
//    @tailrec
//    def r(index: Int, indexIncrementFn: Int => Int, currentMax: Int, resultMax: Int): Int = {
//
//      if ( index < 0 || index > originalPrices.length - 1) {
//        resultMax
//      } else {
//        val price = originalPrices(index)
//        if ( satisfiesConditions(price) ) {
//          val nextCurrentMax = currentMax + 1
//          val nextMax = if ( nextCurrentMax > resultMax ) nextCurrentMax else resultMax
//          r(indexIncrementFn(index), indexIncrementFn, nextCurrentMax, nextMax)
//        } else {
//          resultMax
//        }
//      }
//    }
//
//    val initialMax = 0
//
//    val left = r(query.startIndex, _ - 1, initialMax, initialMax)
//    val right = r(query.startIndex, _ + 1,  initialMax, initialMax)
//    val result = left + right - 1
//    result
//  }

  def calcLengthOfSubArray(originalPrices: Array[Int], query: Query): Int = {

    val margin = query.margin
    val minValue = originalPrices.apply(query.startIndex)
    val maxValue = minValue + margin

    def satisfiesConditions(value: Int): Boolean = {
      minValue <= value && value <= maxValue
    }

    lazy val preProcessed = PerProcessedPrices.runLengthEncoding(originalPrices, query.startIndex, margin, -1)

    @tailrec
    def r(index: Int, indexIncrementFn: Int => Int, currentMax: Int, resultMax: Int): Int = {

      if ( index < 0 || index > preProcessed.prices.length - 1) {
        resultMax
      } else {
        val price = preProcessed.prices(index)
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

    val left = r(preProcessed.startIndex, _ - 1, initialMax, initialMax)
    val right = r(preProcessed.startIndex, _ + 1,  initialMax, initialMax)
    val result = left + right - 1
    result
  }

  object model {

    case class QueryResult(index: Int, value: Int)

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

    object PerProcessedPrices {

      def runLengthEncoding(
        originalPrices: Array[Int],
        originalStartIndex: Int,
        margin: Int,
        compressionValue: Int,
      ): PerProcessedPrices = {

        if ( originalPrices.isEmpty ) {
          PerProcessedPrices(originalPrices.toList, originalStartIndex, compressionValue)
        } else {

          val minValue = originalPrices.apply(originalStartIndex)
          val maxValue = minValue + margin

          def isIndexBeforeOriginalStart(index: Int): Boolean = index < originalStartIndex

          @tailrec
          def r(prices:  List[(Int, Int)], resultPrices: Queue[Int], pricesDroppedBeforeStartIndex: Int): PerProcessedPrices = {
            prices match {
              case Nil =>
                val updatedStartIndex = originalStartIndex - pricesDroppedBeforeStartIndex
                PerProcessedPrices(resultPrices.toList, updatedStartIndex, compressionValue)
              case h :: tail =>
                val value = h._1
                val index = h._2
                if ( value < minValue || value > maxValue) {
                  val (pricesToDrop: List[(Int, Int)], remainingPrices: List[(Int, Int)]) =
                    tail
                      .span(v =>
                        v._1 < minValue || v._1 > maxValue
                      )
                  val nextPricesDropped: Int =
                    if ( isIndexBeforeOriginalStart(index) ) {
                      pricesDroppedBeforeStartIndex + pricesToDrop.length
                    } else {
                      pricesDroppedBeforeStartIndex
                    }

                  r(remainingPrices, resultPrices.enqueue(compressionValue), nextPricesDropped)
                } else {
                  r(tail, resultPrices.enqueue(value), pricesDroppedBeforeStartIndex )
                }

            }
          }

          r(originalPrices.toList.zipWithIndex, Queue(), 0)
        }
      }

    }

    case class ProcessedPrices(prices: List[Int], numOfElemDropped: Int, compressionValue: Int)

    case class PerProcessedPrices(prices: List[Int], startIndex: Int, compressionValue: Int)

    case class Input(
      lengthOfPrices: Int,
      prices: Array[Int],
      numOfQueries: Int,
      queries: List[Query],
    )

    case class QueryOptimization(
      index: Int,
      query: Query,
      value: Int
    )

    case class Query(
      startIndex: Int,
      margin: Int,
    )
  }

}
