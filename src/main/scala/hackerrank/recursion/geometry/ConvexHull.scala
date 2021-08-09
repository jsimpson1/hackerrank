package hackerrank.recursion.geometry

import scala.math.BigDecimal

object ConvexHull {

  def main(args: Array[String]): Unit = {

    val input = ConvexHullInputs.test2

    printPerimeter(input)
  }

  def inputToPoints(input: String): Set[Point] = {
    val inputLines = input.split("\\n").toList
    val numOfPoints = inputLines.head.toInt
    List
      .range(0, numOfPoints)
      .map { i =>
        val r = inputLines.tail(i).split(" ")
        Point(r(0).toDouble, r(1).toDouble)
      }
      .toSet
  }

  def calcPerimeter(input: String): Double = {
    val perimeter = Polygon(inputToPoints(input)).perimeter
    BigDecimal(perimeter)
      .setScale(1, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
  }

  def printPerimeter(input: String): Unit =
    println(s"${calcPerimeter(input)}")


}

case class ConvexHull(points: Set[Point]) extends GeometryPolygon
