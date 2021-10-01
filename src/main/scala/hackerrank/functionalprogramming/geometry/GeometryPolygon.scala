package hackerrank.functionalprogramming.geometry

import GeometryPolygon.allPointsOnPerimeter

import scala.annotation.tailrec

case class Polygon(points: Set[Point]) extends GeometryPolygon

object GeometryPolygon {

  def sortPointsClockwise(points: Set[Point], centroid: Point): List[Point] = {
    points
      .toList
      .sortWith { (a, b) =>
        val a1: Double = (Math.toDegrees(Math.atan2(a.x - centroid.x, a.y - centroid.y)) + 360) % 360;
        val a2: Double = (Math.toDegrees(Math.atan2(b.x - centroid.x, b.y - centroid.y)) + 360) % 360;
        a1 < a2
      }
  }

  def allPointsOnPerimeter(points: Set[Point]): Set[Point] = {
    val polygon = Polygon(points)
    points
      .filter{ point =>
        val isPerimeterCornerPoint = polygon.resolvePerimeterPoints.contains(point)
        val isOnPerimeter = polygon.perimeterLines.exists(_.isPointOnLine(point))
        isPerimeterCornerPoint || isOnPerimeter
      }
  }

}

trait GeometryPolygon {

  def points: Set[Point]

  lazy val centroid: Point = {
    val x: Double = points.map(_.x).sum / points.size
    val y: Double = points.map(_.y).sum / points.size
    Point(x, y)
  }

  val minXCoord: Double =
    points.map(_.x).min

  val maxXCoord: Double =
    points.map(_.x).max

  val maxPoint: Point =
    points
      .filter(_.x == maxXCoord)
      .maxBy(_.y)

  val minPoint: Point =
    points
      .filter(_.x == minXCoord)
      .minBy(_.y)

  def perimeter: Double =
    perimeterLines
      .map(_.length)
      .sum

  def perimeterLines: List[Line] = {
    val sortedPoints = GeometryPolygon.sortPointsClockwise(resolvePerimeterPoints, centroid)
    val firstPoint = sortedPoints.head

    @tailrec
    def r(points: List[Point], result: List[Line]): List[Line] = {
      points match {
        case Nil => result
        case List(p0) =>
          result :+ Line(p0, firstPoint)
        case l =>
          r(points.tail, result :+ Line(l.head, l.tail.head))
      }
    }

    val result = r(sortedPoints, List())
    result
  }

  def resolvePerimeterPoints: Set[Point] =
    resolvePerimeterPoints(Line(minPoint, maxPoint), points)

  def resolvePerimeterPoints(line: Line, allPoints: Set[Point]): Set[Point] = {

    def r(line: Line, aboveBelowFn: (Line, Set[Point]) => Set[Point], remainingPoints: Set[Point], result: Set[Point]): Set[Point] = {
      val points: Set[Point] = aboveBelowFn(line, remainingPoints)
      if (points.isEmpty) {
        result
      } else {
        val furthestPoint = line.pointFurthestFromLine(points)
        val triangle = Triangle(line.p0, line.p1, furthestPoint)
        val nextRemainingPoints = {
          points
            .filterNot { point =>
              triangle.isPointInside(point) || triangle.points.contains(point)
            }
        }
        r(Line(line.p0, furthestPoint), aboveBelowFn, nextRemainingPoints, result ++ triangle.points) ++
          r(Line(line.p1, furthestPoint), aboveBelowFn, nextRemainingPoints, result ++ triangle.points)
      }
    }


    r(line, Line.pointsAboveLine, allPoints, Set()) ++
      r(line, Line.pointsBelowLine, allPoints, Set())
  }

  def allPointsOnPerimeter: Set[Point] =
    GeometryPolygon
      .allPointsOnPerimeter(points)

  def isConcave: Boolean =
    allPointsOnPerimeter.size != points.size


}
