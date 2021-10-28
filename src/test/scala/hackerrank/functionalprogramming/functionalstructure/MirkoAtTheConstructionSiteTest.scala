package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.MirkoAtTheConstructionSite
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class MirkoAtTheConstructionSiteTest extends AnyFunSuite {

  def doTest(inputStr: String, expected: List[Int]): Unit = {

    val actual: List[Int] = MirkoAtTheConstructionSite.solve(inputStr)

    assert(actual == expected)
  }

  def getInputString(caseNum: Int): String = {
    val source = Source.fromFile(s"/Users/flow/code/jeremy/hackerrank/test_cases/functionalstructure/mirkoAtTheConstructionSiteCase${caseNum}.txt")
    source.mkString
  }

  def getExpected(caseNum: Int): List[Int] = {
    val source = Source.fromFile(s"/Users/flow/code/jeremy/hackerrank/test_cases/functionalstructure/mirkoAtTheConstructionSiteCase${caseNum}Result.txt")
    source
      .mkString
      .split("\n")
      .map(_.toInt)
      .toList
  }

  test("case 0") {

    val inputStr = """3 6
                     |7 5 1
                     |1 2 3
                     |0
                     |1
                     |2
                     |3
                     |4
                     |5""".stripMargin

    val expected = List(1, 1, 2, 2, 3, 3)

    doTest(inputStr, expected)
  }

  test("case 6") {

    val caseNum = 6

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doTest(inputStr, expected)
  }

  test("case 7") {

    val caseNum = 7

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doTest(inputStr, expected)
  }

  test("case 18") {

    val caseNum = 18

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doTest(inputStr, expected)
  }


}
