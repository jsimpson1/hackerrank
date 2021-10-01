package hackerrank.recursion.geometry

import hackerrank.functionalprogramming.geometry.{ConvexHull, ConvexHullInputs, Point, Polygon}
import hackerrank.recursion.geometry
import org.scalatest.funsuite.AnyFunSuite

class ConvexHullTest extends AnyFunSuite {

  def pointsTest(testName: String, input: String, expected: Set[Point]): Unit = {
    val points = ConvexHull.inputToPoints(input)
    val actual = Polygon(points).resolvePerimeterPoints
    val msg =
      s"""${testName}
         |expected=${expected.toList.sortBy(v => (v.x, v.y))}
         |actual  =${actual.toList.sortBy(v => (v.x, v.y))}
         |---------------------------------------------------""".stripMargin
    println(msg)
    assertResult(expected)(actual)
  }

  test("case 0 - points"){
    pointsTest("case 0", ConvexHullInputs.test0, Set(
      Point(1, 1),
      Point(2, 5),
      Point(5, 3),
    ))
  }

  test("case 1 - points"){
    pointsTest("case 1", ConvexHullInputs.test1, Set(
      Point(2, 5),
      Point(4, 5),
      Point(3, 2),
    ))
  }

  test("case 2 - points"){
    pointsTest("case 2", ConvexHullInputs.test2, Set(
      Point(950, 991),
      Point(992, 351),
      Point(977, 285),
      Point(917, 26),
      Point(224, 13),
      Point(68, 80),
      Point(5, 341),
      Point(8, 555),
      Point(29, 682),
      Point(143, 995),
    ))
  }

  test("case 2.0 - points"){
    pointsTest("case 2.0", ConvexHullInputs.test2_0, Set(
      Point(950, 991),
      Point(992, 351),
      Point(977, 285),
      Point(917, 26),
      Point(224, 13),
      Point(68, 80),
      Point(5, 341),
      Point(8, 555),
      Point(29, 682),
      Point(143, 995),
    ))
  }


  test("case manual - points"){
    pointsTest("case manual", ConvexHullInputs.testManual, Set(
      Point(0, 2),
      Point(2 ,4),
      Point(3 ,3),
      Point(3 ,1),
      Point(1 ,1),
    ))
  }

  test("case 0 - perimeter"){
    val input = ConvexHullInputs.test0
    val actual = ConvexHull.calcPerimeter(input)
    assertResult(12.2)(actual)
  }

  test("case 1 - perimeter"){
    val input = ConvexHullInputs.test1
    val actual = ConvexHull.calcPerimeter(input)
    assertResult(8.3)(actual)
  }

  test("case 2 - perimeter"){
    val input = ConvexHullInputs.test2
    val actual = ConvexHull.calcPerimeter(input)
    assertResult(3589.2)(actual)
  }

  test("case 2.0 - perimeter"){
    val input = ConvexHullInputs.test2_0
    val actual = ConvexHull.calcPerimeter(input)
    assertResult(3589.2)(actual)
  }



}
