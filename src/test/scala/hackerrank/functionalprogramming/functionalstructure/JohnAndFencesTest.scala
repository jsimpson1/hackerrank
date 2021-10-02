package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.JohnAndFences
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class JohnAndFencesTest extends AnyFunSuite {

  val case0 = Vector(2, 5, 7, 4, 1, 8)

  test("calcAreaToLeft case 0") {

    assert(JohnAndFences.calcAreaToLeft(case0, 0) == 2)
    assert(JohnAndFences.calcAreaToLeft(case0, 1) == 5)
    assert(JohnAndFences.calcAreaToLeft(case0, 2) == 7)
    assert(JohnAndFences.calcAreaToLeft(case0, 3) == 12)
    assert(JohnAndFences.calcAreaToLeft(case0, 4) == 5)
    assert(JohnAndFences.calcAreaToLeft(case0, 5) == 8)

  }

  test("calcAreaToRight case 0") {

    assert(JohnAndFences.calcAreaToRight(case0, 0) == 8)
    assert(JohnAndFences.calcAreaToRight(case0, 1) == 10)
    assert(JohnAndFences.calcAreaToRight(case0, 2) == 7)
    assert(JohnAndFences.calcAreaToRight(case0, 3) == 4)
    assert(JohnAndFences.calcAreaToRight(case0, 4) == 2)
    assert(JohnAndFences.calcAreaToRight(case0, 5) == 8)

  }

  test("calcMaxInteriorRectangleArea case 0") {

    val inputStr = """6
                     |2 5 7 4 1 8""".stripMargin

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(12)(actual)
  }

//  test("calcMaxInteriorRectangleArea case 1") {
//
//    val heights: Vector[Int] = ""
//
//    val actual = JohnAndFences.calcAreaToRight()
//
//  }

  test("calcMaxInteriorRectangleArea case 1") {

    val inputStr = """100
                     |1370 4873 2981 478 4760 5191 6872 6665 3327 3106 9828 9991 208 1667 8408 6876 4872 320 1675 747 7706 4165 1579 2988 1126 2093 1313 5300 2111 6948 6838 9833 1821 6171 310 2932 7713 3533 9596 1039 6639 5775 1030 3198 7441 5789 6425 8665 6108 8099 9411 3814 8616 989 6801 9741 9433 4465 5040 1544 1412 8230 7728 3232 4400 4389 2515 8464 7922 8463 9503 912 589 532 461 4382 6320 6885 3046 2427 1335 8808 2592 6302 6149 5744 6043 5581 208 7434 3476 1620 2015 7555 1203 2766 1944 3718 1230 6217""".stripMargin

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(40523)(actual)
  }

  test("calcMaxInteriorRectangleArea case 4") {

    val inputStr = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/johnAndFencesTestcase4.txt")
      s.mkString
    }

    val actual: Int = JohnAndFences.calcMaxRectangleArea(inputStr)

    assertResult(84883)(actual)
  }

}
