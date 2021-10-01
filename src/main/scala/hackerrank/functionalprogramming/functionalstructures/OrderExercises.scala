package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.model.SegmentedTree

import scala.annotation.tailrec
import scala.language.implicitConversions

object OrderExercises {

  import model._

  def main(args: Array[String]): Unit = {
    val inputStr = """5 3
                  |2 4 -10 2 -2""".stripMargin

    val input = parseInput(inputStr)

    println(input)

    val result = calcSubArrays(input)

    result.print()

  }

  def parseInput(str: String): Input = {
    val lines = str.split("\n")
    val nk = lines.head.split(" ")
    val arr = lines(1).split(" ").map(_.toInt).toList
    Input(nk(0).toInt, nk(1).toInt, arr)
  }

  implicit def sum(a: Int, b: Int): Int = a + b

  def calMaxSumSubArray(segmentedTree: SegmentedTree[Int], startIndex: Int, endIndex: Int): SubArray = {

    @tailrec
    def r(
      temp: SubArray,
      max: SubArray,
    ): SubArray = {
      if (temp.parentEndIndex > endIndex ) {
        max
      } else {
        val currentSum: Int =
          SegmentedTree
            .query(segmentedTree, temp.parentStartIndex, temp.parentEndIndex )

        if ( currentSum > max.sum ) {
          val nextMax = temp.copy(sum = currentSum)
          r(nextMax.incrementEndIndex, nextMax)
        } else if ( currentSum < 0) {
          r(temp.resetSum, max)
        } else {
          r(temp.incrementEndIndex, max)
        }
      }
    }

    val initialSubArray = SubArray(0, startIndex, startIndex)

    r(initialSubArray, initialSubArray)
  }

  def calcSubArrays(input: Input): Result = {

    def emptyValue: Int = 0

    lazy val segmentedTree: SegmentedTree[Int] =
      SegmentedTree
        .build[Int](
          input.arr,
          emptyValue,
        )

    def r(startIndex: Int, endIndex: Int, result: Set[SubArray]): Set[SubArray] = {
      if ( startIndex >= endIndex ) {
        result
      } else {
        val subArray = calMaxSumSubArray(segmentedTree, startIndex, endIndex)

        lazy val nextResult: Set[SubArray] =
          if ( subArray.sum > 0 )
            result + subArray
          else
            result

        r(startIndex, subArray.parentStartIndex - 1, nextResult) ++ r(subArray.parentEndIndex + 1, endIndex, nextResult)
      }
    }

    Result(
      input.k,
      r(0, segmentedTree.maxIndex, Set()).toList
    )
  }

  object model {

    case class Input(n: Int, k: Int, arr: List[Int])

    case class SubArray(
      sum: Int,
      parentStartIndex: Int,
      parentEndIndex: Int
    ) {

      def incrementEndIndex: SubArray =
        this
          .copy(parentEndIndex = parentEndIndex + 1)

      def resetSum: SubArray =
        this
          .copy(sum = -1, parentStartIndex = parentEndIndex + 1, parentEndIndex = parentEndIndex + 1)

    }

    case class Result(k: Int, subArrays: List[SubArray]) {

      def sortedResult: List[Int] =
        subArrays
          .map{v => v.sum }
          .sorted(Ordering[Int].reverse)
          .take(k)

      def print(): Unit =
        println(
          sortedResult
            .mkString("\n")
        )

    }

  }

}
