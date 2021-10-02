package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.JohnAndFences
import hackerrank.functionalprogramming.functionalstructures.data.JohnAndFenceInputs
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class JohnAndFencesTest extends AnyFunSuite {

  val case0 = Vector(2, 5, 7, 4, 1, 8)

  test("calcNumOfFencesToLeft case 0") {

    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 0) == 1)
    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 1) == 1)
    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 2) == 1)
    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 3) == 3)
    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 4) == 5)
    assert(JohnAndFences.calcNumOfFencesToLeftInclusive(case0, 5) == 1)

  }

  test("calcNumOfFencesToRight case 0") {

    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 0) == 4)
    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 1) == 2)
    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 2) == 1)
    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 3) == 1)
    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 4) == 2)
    assert(JohnAndFences.calcNumOfFencesToRightInclusive(case0, 5) == 1)

  }

  test("calcMaxInteriorRectangleArea case 0") {

    val inputStr = """6
                     |2 5 7 4 1 8""".stripMargin

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(12)(actual)
  }

//  test("case 1 reduced vector size") {
//
//    val targetValue = 5789
//
//    val heights: Vector[Int] = Vector(
//      1039,
//      6639,
//      5775,
//      1030,
//      3198,
//      7441,
//      targetValue,
//      6425,
//      8665,
//      6108,
//      8099,
//      9411,
//      3814,
//      8616,
//      989
//    )
//
//    val actualLeft = JohnAndFences.calcNumOfFencesToLeft(heights, 6)
//
//    assert(actualLeft == targetValue * 2)
//
//    val actualRight = JohnAndFences.calcNumOfFencesToRight(heights, 6)
//
//    assert(actualRight == targetValue * 6)
//
//    val solutionValue = 40523
//
//    assert((actualRight + actualLeft - 1) == solutionValue)
//
//  }
//
//  test("calcMaxInteriorRectangleArea case 1 left and right check") {
//
//    val heights: Vector[Int] = JohnAndFenceInputs.case1Heights
//
//    val index = 46
//
////    val actualLeft = JohnAndFences.calcAreaToLeft(heights, index)
////
////    assert(actualLeft == 6425)
//
//    val actualRight = JohnAndFences.calcNumOfFencesToRight(heights, index)
//
//    assert(actualRight == 40523)
//
//  }

  test("calcMaxInteriorRectangleArea case 1") {

    val inputStr = JohnAndFenceInputs.case1

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(40523)(actual)
  }


  test("calcMaxInteriorRectangleArea case 2") {

    val inputStr = JohnAndFenceInputs.case2

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(50216)(actual)
  }

  test("calcMaxInteriorRectangleArea case 4") {

    val inputStr = JohnAndFenceInputs.case4

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(84883)(actual)
  }

}
