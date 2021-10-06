package hackerrank.functionalprogramming.recursion.geometry


object CovextHullDemo extends App {

  //  val line = Line(Point(8,555), Point(29, 682))
  //  val point = Point(20, 604)

  //  val line = Line(Point(68,80), Point(224, 13))
  //  val point = Point(167, 39)
  //
  val line = Line(Point(143, 995), Point(950, 991))
  val point = Point(446, 988)

  println(s"pointsBelowLine:${Line.pointsBelowLine(line, Set(point))}")
  println(s"pointsAboveLine:${Line.pointsAboveLine(line, Set(point))}")
  println(s"pointsOnLIne:${Line.pointsOnLIne(line, Set(point))}")
}
