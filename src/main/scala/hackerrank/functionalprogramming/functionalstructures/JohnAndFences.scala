package hackerrank.functionalprogramming.functionalstructures

import scala.annotation.tailrec

object JohnAndFences {

  import model._
  import scala.math._

  def main(args: Array[String]): Unit = {

    val inputStr = """6
                     |2 5 7 4 1 8""".stripMargin

    val input = parseInput(inputStr)

    println(s"input=$input")

    val maxArea = calcMaxRectangleArea(input)

    println(s"maxArea=$maxArea")

  }

  def parseInput(str: String): Input = {
    val lines = str.split("\n")
    lines match {
      case Array(numOfFences,fenceHeights) =>
        Input(
          numOfFences.toInt,
          fenceHeights
            .split(" ")
            .map(_.toInt)
            .toVector
        )
      case _ =>
        throw new RuntimeException("number of lines for input are incorrect")
    }
  }

  def calcNumberOfFences(
    heights: Vector[Int],
    startIndex: Int,
    indexIncrementFn: Int => Int,
    endCondition: (Int, Int, Vector[Int]) => Boolean,
  ): Int = {
    val maxIndex = heights.size - 1

    if ( startIndex < 0 || startIndex > maxIndex) {
      throw new IndexOutOfBoundsException(s"minIndex=0 and maxIndex=$maxIndex, startIndex=$startIndex")
    } else {

      val initialHeight = heights(startIndex)

      @tailrec
      def r(index: Int, result: Int): Int = {
        if ( endCondition(startIndex, index, heights) ) {
          result
        } else {
          r(indexIncrementFn(index), result + 1)
        }
      }
      r(startIndex, 0)
    }
  }

  def calcNumOfFencesToLeftInclusive(
    heights: Vector[Int],
    startIndex: Int,
  ): Int = {

    def leftEndCondition(
      startIndex: Int,
      currentIndex: Int,
      heights: Vector[Int],
    ): Boolean =
      currentIndex < 0 || heights(currentIndex) < heights(startIndex)

    calcNumberOfFences(heights, startIndex, _ - 1, leftEndCondition)
  }

  def calcNumOfFencesToRightInclusive(
    heights: Vector[Int],
    startIndex: Int,
  ): Int = {

    def rightEndCondition(
      startIndex: Int,
      currentIndex: Int,
      heights: Vector[Int],
    ): Boolean =
      currentIndex > heights.size - 1 || heights(currentIndex) < heights(startIndex)

    calcNumberOfFences(heights, startIndex, _ + 1, rightEndCondition)
  }


  def calcMaxRectangleArea(inputStr: String): Int =
    calcMaxRectangleArea(parseInput(inputStr))

  def calcMaxRectangleArea(input: Input): Int = {

    lazy val heights: Vector[Int] = input.heights

    @tailrec
    def r(index: Int, result: Int): Int = {
      if ( index > heights.size - 1 ) {
        result
      } else {
        val leftFences = calcNumOfFencesToLeftInclusive(heights, index)
        val rightFences = calcNumOfFencesToRightInclusive(heights, index)
        val area = (leftFences+ rightFences - 1) * heights(index)
        val nextResult = if ( area > result ) area else result
        r(index + 1, nextResult)
      }
    }
    r(0, 0)
  }

  object model {

    case class Input(numOfFences: Int, heights: Vector[Int])

  }

}
