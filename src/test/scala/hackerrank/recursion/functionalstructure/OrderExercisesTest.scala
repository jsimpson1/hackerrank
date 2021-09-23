package hackerrank.recursion.functionalstructure

import hackerrank.recursion.functionalstructures.OrderExercises._
import hackerrank.recursion.functionalstructures.OrderExercises.model._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.language.implicitConversions

class OrderExercisesTest extends AnyFunSuite {

  val printResults = true
//  val printResults = false

  def printResults[A](expected: A, actual: A): Unit = {
    if ( printResults )
      println(
        s"""expected = $expected
           |actual   = $actual""".stripMargin
      )
  }

  def pairToValue(v: (Int,Int)): Value =
    Value(v._1, v._2)

  implicit def pairsToValues(v: List[(Int, Int)]): List[Value] =
    v.map(pairToValue)

  test("calMaxSum case 0") {

    val arr = List(-2, -3, 4, -1, -2, 1, 5, -3).zipWithIndex

    val emptySubArr = emptySubArray(0)

    val actual = calMaxSum(arr, emptySubArr, emptySubArr)

    val expected = SubArray(7, 2, 6)

    printResults[SubArray](expected, actual)

    assertResult(expected)(actual)

  }


  test("case 0") {
    val inputStr = """5 3
                 |2 4 -10 2 -2""".stripMargin

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(6,2)

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 1") {
    val inputStr = """4 2
                     |-2 5 -1 -8""".stripMargin

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(5)

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 3") {
    val inputStr = """100 16
                     |9404 8036 -9334 -9146 8085 3024 988 5875 2264 -4643 -8916 -8072 1954 3424 5364 -2633 -8910 -7310 9443 -5096 4982 -7834 5164 -8360 185 265 277 -4154 -6615 6233 5988 -9008 5849 -948 6458 -9633 7955 432 1308 6533 -4667 9545 9446 1002 4452 -2285 -2413 -8734 4224 5492 -6250 -38 -3089 -6761 2326 -2209 -7962 -929 5710 -391 -6415 5399 4758 933 -3318 -8572 566 8181 -1512 -2937 -5897 5525 -7054 912 -8863 -4893 2963 -8827 -8376 5579 -8906 2265 5349 9388 4664 5708 2630 -4177 7665 6774 -5152 -5504 6138 2018 2464 3936 -5985 9804 -9520 1245""".stripMargin

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(
      47985,
      40945,
      20236,
      17440,
      11090,
      10742,
      9716,
      9443,
      8747,
      5710,
      5579,
      5525,
      5164,
      4982,
      2963,
      2326,
    )

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 6") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase6.txt")
      s.mkString
    }

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase6Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    }

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 7") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase7.txt")
      s.mkString
    }

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase7Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    }

    println(s"actual diff expected ${actual.diff(expected)}")

    println(s"expected diff actual ${expected.diff(actual)}")

    assertResult(expected)(actual)
  }

  test("case 16") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase16.txt")
      s.mkString
    }

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase16Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    } ++ List(136, 126, 99, 93, 53, 49, 49)

//    printResults(expected, actual)

    println(s"actual diff expected ${actual.diff(expected)}")

    println(s"expected diff actual ${expected.diff(actual)}")

    assertResult(expected)(actual)
  }

}
