package hackerrank.recursion.geometry

import scala.math.BigDecimal.RoundingMode
import scala.math.{BigDecimal, sqrt}

object Triangle {

  def altitude(line: Line, point: Point): Double = {
    val area0 = area(line.p0, line.p1, point)
    val base = line.length
    2 * area0 / base
  }

  def area(line: Line, point: Point): Double =
    area(line.p0, line.p1, point)

  def area(p0: Point, p1: Point, p2: Point): Double = {
    val a = Line(p0, p1).length
    val b = Line(p1, p2).length
    val c = Line(p2, p0).length
    0.25 * sqrt((a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c))
  }
}

case class Triangle(p0: Point, p1: Point, p2: Point) {

  def areaBd(d: Double): Double = BigDecimal(d).setScale(4, RoundingMode.HALF_UP).toDouble

  lazy val area: Double = areaBd(Triangle.area(p0, p1, p2))

  def isPointInside(point: Point): Boolean = {
    val area0 = Triangle(p0, p1, point).area
    val area1 = Triangle(p1, p2, point).area
    val area2 = Triangle(p2, p0, point).area
    val areaMatch = area == (area0 + area1 + area2)
    areaMatch && !points.contains(point)
  }

  def points: Set[Point] = Set(p0, p1, p2)

}
