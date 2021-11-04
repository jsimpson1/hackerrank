package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.MirkoAtTheConstructionSite._
import hackerrank.functionalprogramming.functionalstructures.MirkoAtTheConstructionSite.model._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class MirkoAtTheConstructionSiteTest extends AnyFunSuite {


  def doSolveTest(inputStr: String, expected: List[Int]): Unit = {

    val actual: List[Int] = solve(inputStr)

    assert(actual == expected)
  }

  def doReduceBuildingsTest(caseNum: Int, numOfReductions: Int): Unit = {

    val input = Input.parse(getInputString(caseNum))

    val firstStepOfInitializedBuildings =
      reduceNumOfBuildingsByGrowthRate(
        Building
          .createBuildings(input)
      )

    val actual =
      reduceNumOfBuildingsByIntersection(input.maxDay, firstStepOfInitializedBuildings, numOfReductions).map(_.index)

    val expected: List[Int] =
      getExpected(caseNum)
        .distinct

    println(
      s"""doReduceBuildingsTest --
         |  originalBuildingLength         =${input.initialFloorHeights.length}
         |  firstStepOfInitializedBuildings=${firstStepOfInitializedBuildings.length}
         |  actualLength                   =${actual.length}
         |  expectedLength                 =${expected.length}""".stripMargin)

    // Check that each expected answer is in the reduced list
    expected
      .foreach { index =>
        val actualWithIndex = (index, actual.contains(index))
        assert(actualWithIndex == (index, true))
      }
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

  test("reduceNumOfBuildingsByIntersection -- case 2") {
    val caseNum = 2
    val numOfReductions = 4
    doReduceBuildingsTest(caseNum, numOfReductions)
  }

  test("reduceNumOfBuildingsByIntersection -- case 6") {
    val caseNum = 6
    val numOfReductions = 4
    doReduceBuildingsTest(caseNum, numOfReductions)
  }

  test("reduceNumOfBuildingsByIntersection -- case 7") {
    val caseNum = 7
    val numOfReductions = 4
    doReduceBuildingsTest(caseNum, numOfReductions)
  }

  test("reduceNumOfBuildingsByIntersection -- case 18") {
    val caseNum = 18
    val numOfReductions = 4
    doReduceBuildingsTest(caseNum, numOfReductions)
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

    doSolveTest(inputStr, expected)
  }

  test("case 2") {

    val caseNum = 2

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doSolveTest(inputStr, expected)
  }

  test("case 6") {

    val caseNum = 6

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doSolveTest(inputStr, expected)
  }

  test("case 7") {

    val caseNum = 7

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doSolveTest(inputStr, expected)
  }

  test("case 18") {

    val caseNum = 18

    val inputStr: String = getInputString(caseNum)

    val expected: List[Int] = getExpected(caseNum)

    doSolveTest(inputStr, expected)
  }


}
