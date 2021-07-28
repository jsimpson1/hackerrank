package hackerrank

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode
import scala.math._

object ConvexHull {

  def main(args: Array[String]): Unit = {

    val input = ConvexHullInputs.testManual

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

  def calcPerimeter(input: String): Double =  {
    val p = calcConvexHull(inputToPoints(input)).perimeter
    val v = BigDecimal(p).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    v
  }

  def printPerimeter(input: String): Unit = {
    println(f"${calcPerimeter(input)}%1.1f")
  }

  object Point {
    def apply(x: Int, y: Int): Point = Point(x.toDouble, y.toDouble)
  }

  case class Point(x: Double, y: Double)

  case class Line(p0: Point, p1: Point) {
    def slope: Double =
      (p1.y - p0.y)/(p1.x - p0.x)

    def yIntercept: Double =
      p1.y - (slope * p1.x)

    def length: Double =
      sqrt(pow(p1.x - p0.x, 2) + pow(p1.y - p0.y, 2))

    def pointFurthestFromLine(points: Set[Point]): Point = {
//      println(s"pointFurthestFromLine -- size=${points.size}")
      points
        .map{ point =>
          val a = Triangle.altitude(this, point)
          (point, a)
        }.maxBy(_._2)
        ._1
    }

    def pointsAboveLine(points: Set[Point]): Set[Point] =
      points
        .filter{ point =>
          point.y > (slope * point.x) + yIntercept
        }

    def pointsBelowLine(points: Set[Point]): Set[Point] =
      points
        .filter{ point =>
          point.y < (slope * point.x) + yIntercept
        }

    def pointsOnLIne(points: Set[Point]): Set[Point] =
      points
        .filter{ point =>
          point.y == (slope * point.x) + yIntercept
        }

    def isPointOnLine(point: Point): Boolean =
      pointsOnLIne(Set(point)).nonEmpty

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
      0.25 * sqrt((a + b + c)*(-a + b + c)*(a - b + c)*(a + b -c))
    }
  }

  case class Triangle(p0: Point, p1: Point, p2: Point) {

    def areaBd(d: Double) = BigDecimal(d).setScale(4, RoundingMode.HALF_UP).toDouble

    lazy val area: Double = areaBd(Triangle.area(p0, p1, p2))

    def isPointInside(point: Point): Boolean = {
      val area0 = Triangle(p0, p1, point).area
      val area1 = Triangle(p1, p2, point).area
      val area2 = Triangle(p2, p0, point).area
//      println(s"isPointInside -- p0=${p0}, p1=${p1}, point=${point}")
//      println(s"isPointInside -- area0=${area0}, area1=${area1}, area2=${area2} ... area=${area}")
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
        .sortWith{ (a, b) =>
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
            r(points.tail, result +  p)
          case l =>
            val p = Line(l.head, l.tail.head).length
            r(points.tail, result +  p)
        }
      }
      r(sortedPoints, 0.0)
    }

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


    def resolvePerimeterPoints(line: Line, allPoints: Set[Point]): Set[Point] = {

      def resolveRemainingPoints(line: Line, isAbove: Boolean, points: Set[Point]): Set[Point] =
        if ( isAbove ) {
          line.pointsAboveLine(points)
        } else {
          line.pointsBelowLine(points)
        }


      def r(line: Line, isAbove: Boolean, remainingPoints: Set[Point], result: Set[Point]): Set[Point] = {
        val points = resolveRemainingPoints(line, isAbove, remainingPoints)
        if ( points.isEmpty) {
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
            s"""resolvePerimeterPoints -- ${if (isAbove) "above" else "below"}
               |remainingPoints=${remainingPoints}
               |line=${line}
               |furthestPoint=${furthestPoint}
               |triangle=${triangle}
               |nextRemainingPoints=${nextRemainingPoints}
               |""".stripMargin
          println(testOutput)
          r(Line(line.p0, furthestPoint), isAbove, nextRemainingPoints, result ++ triangle.points)++
          r(Line(line.p1, furthestPoint), isAbove, nextRemainingPoints, result ++ triangle.points)
        }
      }

      val pointsAbove = line.pointsAboveLine(allPoints)
      val pointsBelow = line.pointsBelowLine(allPoints)

      println(s"resolvePerimeterPoints -- initialLine:${line}")
      println(s"resolvePerimeterPoints -- pointsAbove:${pointsAbove}")
      println(s"resolvePerimeterPoints -- pointsBelow:${pointsBelow}\n---------------------------")

      r(line, isAbove = true, pointsAbove, Set()) ++
      r(line, isAbove = false, pointsBelow, Set())
    }

    ConvexHull(
      resolvePerimeterPoints(
        line = Line(minPoint, maxPoint),
        allPoints = allPoints
      )
    )
  }

}
