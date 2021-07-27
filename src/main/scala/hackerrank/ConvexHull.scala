package hackerrank

import scala.math._

object ConvexHull {

  def main(args: Array[String]): Unit = {
    //    val input = """6
    //                  |1 1
    //                  |2 5
    //                  |3 3
    //                  |5 3
    //                  |3 2
    //                  |2 2""".stripMargin

    val input = ConvexHullInputs.test0

    printPerimeter(input)
  }

  def calcPerimeter(input: String): Double = try {
    val inputLines = input.split("\\n").toList
    val numOfPoints = inputLines.head.toInt
    val points: Set[Point] =
      List
        .range(0, numOfPoints)
        .map { i =>
          val r = inputLines.tail(i).split(" ")
          Point(r(0).toInt, r(1).toInt)
        }
        .toSet
    calcConvexHull(points).perimeter
  } catch {
    case e: Exception =>
      println(s"error - ${e.getMessage}")
      0.0
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
      (p1.y - p0.y) / (p1.x - p0.x)

    def yIntercept: Double =
      p1.y - (slope * p1.x)

    def length: Double =
      sqrt(pow(p1.x - p0.x, 2) + pow(p1.y - p0.y, 2))

    def pointFurthestFromLine(points: Set[Point]): Point =
      points
        .map { point =>
          (point, Triangle.altitude(this, point))
        }.maxBy(_._2)
        ._1

    def pointsAboveLine(points: Set[Point]): Set[Point] =
      points
        .filter { point =>
          point.y > (slope * point.x) + yIntercept
        }

    def pointsBelowLine(points: Set[Point]): Set[Point] =
      points
        .filter { point =>
          point.y < (slope * point.x) + yIntercept
        }

    def pointsOnLIne(points: Set[Point]): Set[Point] =
      points
        .filter { point =>
          point.y == (slope * point.x) + yIntercept
        }

  }

  object Triangle {

    // A = 0.5bh
    def altitude(line: Line, point: Point): Double = {
      val area0 = area(line.p0, line.p1, point)
      val base = line.length
      2 * area0 / base
    }

    def area(line: Line, point: Point): Double =
      area(line.p0, line.p1, point)

    // area = 0.25 * √((a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c))
    def area(p0: Point, p1: Point, p2: Point): Double = {
      val a = Line(p0, p1).length
      val b = Line(p1, p2).length
      val c = Line(p2, p0).length
      0.25 * sqrt((a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c))
    }
  }

  case class Triangle(p0: Point, p1: Point, p2: Point) {

    lazy val area: Double = Triangle.area(p0, p1, p2)

    def isPointInside(point: Point): Boolean = {
      val area0 = Triangle(p0, p1, point).area
      val area1 = Triangle(p1, p2, point).area
      val area2 = Triangle(p2, p0, point).area
      area == (area0 + area1 + area2)
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

  /*
    1) Find the points with minimum and maximum x coordinates, as these will always be part of the convex hull.
      If many points with the same minimum/maximum x exist, use ones with minimum/maximum y correspondingly.
    2) Use the line formed by the two points to divide the set in two subsets of points, which will be processed recursively.
    3) Determine the point, on one side of the line, with the maximum distance from the line. This point forms a triangle with those of the line.
    4) The points lying inside of that triangle cannot be part of the convex hull and can therefore be ignored in the next steps.
    5) Repeat the previous two steps on the two lines formed by the triangle (not the initial line).
    6) Keep on doing so on until no more points are left, the recursion has come to an end and the points selected constitute the convex hull.
   */
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

    def resolvePerimeterPoints(line: Line, remainingPoints: Set[Point], result: Set[Point]): Set[Point] = {
      if (remainingPoints.isEmpty) {
        result
      } else {
        val furthestPoint = line.pointFurthestFromLine(remainingPoints)
        val triangle = Triangle(line.p0, line.p1, furthestPoint)
        val remainingPoints0 = remainingPoints
          .filter(point =>
            triangle.isPointInside(point)
          )
        val above: Set[Point] = resolvePerimeterPoints(Line(line.p0, furthestPoint), remainingPoints0, result ++ triangle.points)
        val below: Set[Point] = resolvePerimeterPoints(Line(line.p1, furthestPoint), remainingPoints0, result ++ triangle.points)
        above ++ below
      }
    }

    ConvexHull(
      resolvePerimeterPoints(
        line = Line(minPoint, maxPoint),
        remainingPoints = allPoints.filterNot(p => p == maxPoint || p == minPoint),
        result = Set()
      )
    )
  }

}
