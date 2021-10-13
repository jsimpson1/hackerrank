package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.model.{SegmentedTree, SqrtDecomposition, SqrtDecompositionValue}

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, TreeMap, TreeSet}
import scala.language.implicitConversions
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

//  def calcLengthOfSubArray(originalPrices: Array[Int], query: Query): Int = {
//
//    val margin = query.margin
//    val minValue = originalPrices.apply(query.startIndex)
//    val maxValue = minValue + margin
//
//    def satisfiesConditions(blockMinMax: Int): Boolean = {
//      minValue <= blockMinMax && blockMinMax <= maxValue
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

  implicit class MinMaxOps[MinMax](value: MinMax) {

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

//    case class SqrtDecomposition(values: Array[Int]) {
//
//      val maxIndex: Int = values.length - 1
//
//      val blockSize: Int = sqrt(values.length).toInt
//
//      val valueBlocks: Array[MinMax] =
//        values
//          .sliding(blockSize, blockSize)
//          .map(v =>
//            MinMax(v.min, v.max)
//          ).toArray
//
//
//      def partialBlockCalc(indexes: List[Int]): Option[MinMax] = {
//        @tailrec
//        def r(indexes: List[Int], result: MinMax): MinMax = {
//          indexes match {
//            case Nil =>
//              result
//            case h :: tail =>
//              val value = values(h)
//              val nextResult: MinMax =
//                result
//                  .copy(
//                    min = scala.math.min(result.min, value),
//                    max = scala.math.max(result.max, value)
//                  )
//              r(
//                tail,
//                nextResult
//              )
//          }
//        }
//
//        values match {
//          case Array() =>
//            None
//          case values =>
//            indexes match {
//              case Nil =>
//                None
//              case indexes =>
//                Some(
//                  r(indexes, MinMax(values(indexes.head)))
//                )
//            }
//
//        }
//      }
//
//      def fullBlocksCalc(indexes: List[Int]): Option[MinMax] = {
//
//        @tailrec
//        def r(indexes: List[Int], result: MinMax): MinMax = {
//          indexes match {
//            case Nil =>
//              result
//            case h :: tail =>
//              val block: MinMax = valueBlocks(h)
//              val nextResult = result.combine(block)
//              r(
//                tail,
//                nextResult
//              )
//          }
//        }
//
//        valueBlocks match {
//          case Array() =>
//            None
//          case values =>
//            indexes match {
//              case Nil =>
//                None
//              case indexes =>
//                Some(
//                  r(indexes, values(indexes.head))
//                )
//            }
//        }
//
//      }
//
//      def queryIndexes(startIndex: Int, endIndex: Int): Option[Indexes] = {
//
//        if ( startIndex < 0 || endIndex > maxIndex || endIndex < startIndex) {
//          None
//        } else {
//          val rawIndexes = (startIndex to endIndex).toList
//          val firstBlockStart = rawIndexes.indexWhere(_%blockSize == 0)
//          firstBlockStart match {
//            case -1 =>
//              Some(Indexes(rawIndexes, Nil))
//            case blockStart =>
//              val (firstPartialBlock: List[Int], rest: List[Int]) = rawIndexes.splitAt(blockStart)
//
//              val (fullBlocks, lastPartialBlock) =
//                rest
//                  .sliding(blockSize, blockSize)
//                  .toList match {
//                    case remainingBlocks =>
//                      remainingBlocks match {
//                        case blocks :+ lastBlock =>
//                          if ( lastBlock.size == blockSize ) {
//                            (remainingBlocks, Nil)
//                          } else {
//                            (blocks, lastBlock)
//                          }
//                      }
//
//                  }
//
//              lazy val blockIndexAdjustment: Int =
//                rest.head/blockSize
//
//              val blockIndexes: List[Int] =
//                fullBlocks
//                  .indices
//                  .map(index =>
//                    index + blockIndexAdjustment
//                  ).toList
//
//              Some(
//                Indexes(
//                  firstPartialBlock ++ lastPartialBlock,
//                  blockIndexes
//                )
//              )
//          }
//        }
//
//
//      }
//
//      def query(startIndex: Int, endIndex: Int): Option[MinMax] = {
//
//        if ( startIndex < 0 || endIndex > maxIndex || endIndex < startIndex) {
//          None
//        } else {
//          queryIndexes(startIndex, endIndex) match {
//            case None =>
//              None
//            case Some(indexes) =>
//              val result: Option[MinMax] =
//                indexes match {
//                  case Indexes(Nil, Nil) =>
//                    None
//                  case Indexes(pricesIndexes, Nil) =>
//                    partialBlockCalc(pricesIndexes)
//                  case Indexes(Nil, blockIndexes) =>
//                    fullBlocksCalc(blockIndexes)
//                  case Indexes(partialBlockIndexes, fullBlockIndexes) =>
//                    partialBlockCalc(partialBlockIndexes)
//                      .flatMap { partial =>
//                        fullBlocksCalc(fullBlockIndexes)
//                          .map { full =>
//                            partial.combine(full)
//
//                          }
//                      }
//                }
//              result
//          }
//        }
//
//      }
//
//    }
//
//    case class Indexes(individualCalc: List[Int], block: List[Int])

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
