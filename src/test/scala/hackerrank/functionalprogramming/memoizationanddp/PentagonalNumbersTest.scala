package hackerrank.functionalprogramming.memoizationanddp

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class PentagonalNumbersTest extends AnyFunSuite {

  def doTest(inputStr: String, expectedStr: String): Unit = {

    val input = PentagonalNumbers.model.Input.parseInput(inputStr)

    val actual: Seq[(Long, Int)] =
      input
        .pentagonalNumber
        .zipWithIndex
        .map(num =>
          (PentagonalNumbers.calculatePentagonalNumber(num._1), num._2)
        )

    val expected: Array[(Long, Int)] =
      expectedStr
        .split("\n")
        .map(_.toLong)
        .zipWithIndex

    actual
      .zip(expected)
      .foreach { case (a,e) =>
        assert(a == e)
      }

  }

  test("case 0") {

    val inputStr = """5
                     |1
                     |2
                     |3
                     |4
                     |5""".stripMargin


    val expectedStr = """1
                     |5
                     |12
                     |22
                     |35""".stripMargin

    doTest(inputStr, expectedStr)
  }

  test("case 5") {

    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/memoizationanddp/PentagonalNumbersCase5.txt")
      s.mkString
    }

    val expectedStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/memoizationanddp/PentagonalNumbersCase5Result.txt")
      s.mkString
    }

    doTest(inputStr, expectedStr)
  }


}
