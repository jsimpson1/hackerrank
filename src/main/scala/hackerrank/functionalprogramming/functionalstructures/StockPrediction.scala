package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.model.{SegmentedTree, SqrtDecomposition, SqrtDecompositionValue}

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

    val results = calcLengthOfSubArray(input)

    println(results)

  }


  implicit val minMaxValue: SqrtDecompositionValue[MinMax] = new SqrtDecompositionValue[MinMax] {

    override def apply(i: Int): MinMax =
      MinMax(i, i)

    override def makeBlock(l: Array[Int]): MinMax =
      MinMax(l.min, l.max)

    override def aggregate(a: MinMax, b: MinMax): MinMax =
      MinMax(
        scala.math.min(a.min, b.min),
        scala.math.max(a.max, b.max)
      )
  }

  def calcLengthOfSubArray(input: Input): List[Int] = {

    val preProcessedPrices: SqrtDecomposition[MinMax] =
      SqrtDecomposition(input.prices)

    input
      .queries
      .map { query =>
        calcLengthOfSubArray(preProcessedPrices, query)
      }
  }

  implicit def indexValue(i: Index): Int = i.value

  def calcLengthOfSubArray(preProcessPrices: SqrtDecomposition[MinMax], query: Query): Int = {

    val minValue = preProcessPrices.values(query.startIndex)
    val maxValue = minValue + query.margin

    val queryLimit = MinMax(minValue, maxValue)

    @tailrec
    def r(startIndex: Index, endIndex: Index, result: Int): Int = {
      preProcessPrices
        .query(startIndex, endIndex) match {
          case None =>
            result
          case Some(value) =>
            if ( value.isInside(queryLimit) ) {
              r(startIndex.next, endIndex.next, result + 1)
            } else {
              result
            }
        }
    }
    val left = r(Index(query.startIndex - 1, -1), Index(query.startIndex), 0)
    val right = r(Index(query.startIndex), Index(query.startIndex + 1, 1), 0)
    left + right + 1
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

    object MinMax {

      def apply(value: Int): MinMax =
        MinMax(value, value)

    }

    case class MinMax(min: Int, max: Int) {

      def isInside(value: Int): Boolean =
        value >= min && value <= max

      def isInside(limit: MinMax): Boolean =
        min >= limit.min && max <= limit.max

      def combine(other: MinMax): MinMax =
        MinMax(
          scala.math.min(min, other.min),
          scala.math.max(max, other.max)
        )

    }

    object Index {

      def apply(i: Int): Index =
        Index(i, 0)

    }

    case class Index(value: Int, increment: Int) {
      def next: Index = Index(value + increment, increment)
    }

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
