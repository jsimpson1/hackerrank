package hackerrank.recursion.geometry

import scala.math.{pow, sqrt}

object Line {

  def pointsAboveLine(line: Line, points: Set[Point]): Set[Point] =
    points
      .filter { point =>
        point.y > (line.slope * point.x) + line.yIntercept
      }

  def pointsBelowLine(line: Line, points: Set[Point]): Set[Point] =
    points
      .filter { point =>
        point.y < (line.slope * point.x) + line.yIntercept
      }

  def pointsOnLIne(line: Line, points: Set[Point]): Set[Point] =
    points
      .filter { point =>
        point.y == (line.slope * point.x) + line.yIntercept
      }

}

case class Line(p0: Point, p1: Point) {
  def slope: Double =
    (p1.y - p0.y) / (p1.x - p0.x)

  def yIntercept: Double =
    p1.y - (slope * p1.x)

  def length: Double =
    sqrt(pow(p1.x - p0.x, 2) + pow(p1.y - p0.y, 2))

  def pointFurthestFromLine(points: Set[Point]): Point =
    points
      .map { point =>
        val a = Triangle.altitude(this, point)
        (point, a)
      }.maxBy(_._2)
      ._1

  def isPointOnLine(point: Point): Boolean =
    Line.pointsOnLIne(this, Set(point)).nonEmpty

}
