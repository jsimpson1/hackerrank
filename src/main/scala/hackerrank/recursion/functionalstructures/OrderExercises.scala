package hackerrank.recursion.functionalstructures

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

  def emptySubArray(index: Int): SubArray = SubArray(0,index,index)

  @tailrec
  def calMaxSum(arr: List[Value], current: SubArray, max: SubArray): SubArray = {
    arr match {
      case Nil =>
        max
      case h :: tail =>
        val newCurrent: SubArray =
          current
            .copy(
              sum = current.sum + h.value,
              parentArrayStartIndex = if (current.sum > 0 ) current.parentArrayStartIndex else h.index,
              parentArrayEndIndex = h.index
            )

        val newMax: SubArray =
          if ( newCurrent.sum > max.sum )
            newCurrent
          else
            max

        if ( current.sum < newCurrent.sum) {
          calMaxSum(tail, newCurrent, newMax)
        } else if ( newCurrent.sum < 0 ) {
          calMaxSum(tail, emptySubArray(h.index), newMax)
        } else {
          calMaxSum(tail, newCurrent, newMax)
        }
    }
  }

  def calcSubArrays(input: Input): Result = {

    @tailrec
    def r(arr: List[Value], maxArray: SubArray, result: Result): Result = {
      arr match {
        case Nil =>
          result
        case values =>
          val max: SubArray = calMaxSum(values, maxArray, maxArray)

          if ( max.sum > 0 ) {

            r(
              values.filterNot(v =>
                v.index >= max.parentArrayStartIndex && v.index <= max.parentArrayEndIndex
              ),
              emptySubArray(maxArray.parentArrayStartIndex),
              result.copy(subArrays = max :: result.subArrays)
            )
          } else {
            r(
              arr.filter(_.value > 0),
              emptySubArray(maxArray.parentArrayStartIndex),
              result
            )
          }
      }
    }

    r(
      input.arr.zipWithIndex.map(v => Value(v._1, v._2) ),
      emptySubArray(0),
      Result(input.k, Nil)
    )
  }

  object model {

    case class Input(n: Int, k: Int, arr: List[Int])

    case class Value(value: Int, index: Int)

    case class SubArray(
      sum: Int,
      parentArrayStartIndex: Int,
      parentArrayEndIndex: Int
    ) {

      def isEmpty: Boolean =
        parentArrayStartIndex == parentArrayEndIndex

    }

    case class Result(k: Int, subArrays: List[SubArray]) {

      lazy val sums: List[Int] =
        subArrays
          .map(_.sum)

      def sortedResult: List[Int] =
        subArrays
          .map{v => v.sum }
          .sorted(Ordering[Int].reverse)
          .take(k)

      def print(): Unit =
        println(sortedResult.mkString("\n"))

    }

  }

}
