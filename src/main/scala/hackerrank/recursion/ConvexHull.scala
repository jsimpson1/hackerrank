package hackerrank.recursion

import scala.math.BigDecimal.RoundingMode
import scala.math.{BigDecimal, pow, sqrt}

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
    val perimeter = calcConvexHull(inputToPoints(input)).perimeter
    BigDecimal(perimeter)
      .setScale(1, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
  }

  def printPerimeter(input: String): Unit =
    println(s"${calcPerimeter(input)}")

  case class Point(x: Double, y: Double)

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

  case class ConvexHull(points: Set[Point]) {

    lazy val centroid: Point = {
      val x: Double = points.map(_.x).sum / points.size
      val y: Double = points.map(_.y).sum / points.size
      Point(x, y)
    }

    def sortPointsClockwise: List[Point] = {
      points
        .toList
        .sortWith { (a, b) =>
          val a1: Double = (Math.toDegrees(Math.atan2(a.x - centroid.x, a.y - centroid.y)) + 360) % 360;
          val a2: Double = (Math.toDegrees(Math.atan2(b.x - centroid.x, b.y - centroid.y)) + 360) % 360;
          a1 < a2
        }
    }

    def perimeter: Double = {
      val sortedPoints = sortPointsClockwise
      val firstPoint = sortedPoints.head

      def r(points: List[Point], result: Double): Double = {
        points match {
          case Nil => result
          case List(p0) =>
            val p = Line(p0, firstPoint).length
            r(points.tail, result + p)
          case l =>
            val p = Line(l.head, l.tail.head).length
            r(points.tail, result + p)
        }
      }

      r(sortedPoints, 0.0)
    }

  }

  def resolvePerimeterPoints(line: Line, allPoints: Set[Point]): Set[Point] = {

    def r(line: Line, aboveBelowFn: (Line, Set[Point]) => Set[Point], remainingPoints: Set[Point], result: Set[Point]): Set[Point] = {
      val points = aboveBelowFn(line, remainingPoints)
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
        val testOutput =
        //            s"""resolvePerimeterPoints -- ${if (isAbove) "above" else "below"}
          s"""resolvePerimeterPoints -- }
             |remainingPoints=${remainingPoints}
             |line=${line}
             |furthestPoint=${furthestPoint}
             |triangle=${triangle}
             |nextRemainingPoints=${nextRemainingPoints}
             |""".stripMargin
        println(testOutput)
        r(Line(line.p0, furthestPoint), aboveBelowFn, nextRemainingPoints, result ++ triangle.points) ++
          r(Line(line.p1, furthestPoint), aboveBelowFn, nextRemainingPoints, result ++ triangle.points)
      }
    }


    r(line, Line.pointsAboveLine, allPoints, Set()) ++
      r(line, Line.pointsBelowLine, allPoints, Set())
  }

  def calcConvexHull(allPoints: Set[Point]): ConvexHull = {
    val minXCoord = allPoints.map(_.x).min
    val maxXCoord = allPoints.map(_.x).max

    val maxPoint: Point =
      allPoints
        .filter(_.x == maxXCoord)
        .maxBy(_.y)

    val minPoint: Point =
      allPoints
        .filter(_.x == minXCoord)
        .minBy(_.y)

    ConvexHull(
      resolvePerimeterPoints(
        line = Line(minPoint, maxPoint),
        allPoints = allPoints
      )
    )
  }

}
