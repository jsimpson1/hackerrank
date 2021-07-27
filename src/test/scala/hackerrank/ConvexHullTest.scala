package hackerrank

import org.scalatest.funsuite.AnyFunSuite


class ConvexHullTest extends AnyFunSuite {

  test("case 0"){
    val input = ConvexHullInputs.test0
    val actual = ConvexHull.calcPerimeter(input)
    assertResult(12.2)(actual)
  }

//  test("case 1"){
//    val input = ConvexHullInputs.test1
//    val actual = ConvexHull.calcPerimeter(input)
//    assertResult(8.3)(actual)
//  }
//
//  test("case 2"){
//    val input = ConvexHullInputs.test2
//    val actual = ConvexHull.calcPerimeter(input)
//    assertResult(3589.2)(actual)
//  }

}
