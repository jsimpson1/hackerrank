package hackerrank.recursion.geometry

import hackerrank.functionalprogramming.geometry.{ConcaveInputs, ConcavePolygon, ConvexHullInputs, Polygon}
import org.scalatest.funsuite.AnyFunSuite

class ConcavePolygonTest extends AnyFunSuite {

  test("concave test -- square test0") {
    val str = ConcaveInputs.test0
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = false
    assertResult(expected)(actual)
  }

  test("concave test -- triangle test1") {
    val str = ConcaveInputs.test1
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = false
    assertResult(expected)(actual)
  }

  test("concave test -- complex polygon test2") {
    val str = ConcaveInputs.test2
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = false
    assertResult(expected)(actual)
  }

  test("concave test -- square with point on line test3SecretCase") {
    val str = ConcaveInputs.test3SecretCase
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = false
    assertResult(expected)(actual)
  }

  test("convex test -- triangle test1"){
    val str = ConvexHullInputs.test1
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = false
    assertResult(expected)(actual)
  }

  test("convex test -- has interior points test0"){
    val str = ConvexHullInputs.test0
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = true
    assertResult(expected)(actual)
  }

  test("convex test -- has interior points test2"){
    val str = ConvexHullInputs.test2
    val points = ConcavePolygon.inputToPoints(str)
    val actual = Polygon(points).isConcave
    val expected = true
    assertResult(expected)(actual)
  }



}
