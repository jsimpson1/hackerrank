package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructure.data.JohnAndFenceInputs
import hackerrank.functionalprogramming.functionalstructures.JohnAndFences
import org.scalatest.funsuite.AnyFunSuite

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
