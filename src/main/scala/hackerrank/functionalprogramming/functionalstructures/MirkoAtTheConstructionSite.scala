package hackerrank.functionalprogramming.functionalstructures

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, TreeMap}

object MirkoAtTheConstructionSite {

  import model._
  import scala.io.StdIn._

  def main(args: Array[String]): Unit = {

    val inputStr = """3 6
                     |7 5 1
                     |1 2 3
                     |0
                     |1
                     |2
                     |3
                     |4
                     |5""".stripMargin

    val input = Input.parse(inputStr)

    println(s"input=$input")

  }

  def solve(): Unit = {
    solve(Input.parse())
      .foreach( maxIndex =>
        println(maxIndex)
      )
  }

  def solve(str: String): List[Int] = {
    solve(Input.parse(str))
  }

  def solve(input: Input): List[Int] = {

    lazy val buildings: List[Building] = initializeBuildings(input, 4)

    input
      .dayQueries
      .map{ day =>
        getHighestBuildingMaxIndex(day, buildings, Result.empty)
      }
      .toList
  }

  def reduceNumOfBuildingsByGrowthRate(buildings: List[Building]): List[Building] = {
    buildings
      .groupBy(_.growthRate)
      .map { case (_, buildings) =>
        buildings
          .maxBy(_.initialHeight)
      }.toList
  }

  def intersection(
    day: Int,
    shorterBuilding: Building,
    tallerBuilding: Building
  ): Boolean = {
    shorterBuilding.height(day) >= tallerBuilding.height(day)
  }

  def reduceNumOfBuildingsByIntersection(day: Int, buildings: List[Building], numOfReductions: Int): List[Building] = {

//    @tailrec
//    def checkIntersections(building: Building, buildings: List[Building]): (Building, List[Building]) = {
//      buildings match {
//        case Nil =>
//          (building, Nil)
//        case h :: tail =>
//          if ( intersection(day, building, h) ) {
//            checkIntersections(building, tail)
//          } else {
//            (building, buildings)
//          }
//      }
//    }
//
//    @tailrec
//    def r(buildings: List[Building], result: List[Building]): List[Building] = {
//      buildings match {
//        case Nil =>
//          result
//        case h :: tail =>
//          val intersectionResult = checkIntersections(h, tail)
//          r(intersectionResult._2, intersectionResult._1 :: result)
//      }
//    }

    @tailrec
    def r(buildings: List[Building], result: List[Building]): List[Building] = {
      buildings match {
        case Nil =>
          result
        case b0 :: Nil =>
          b0 :: result
        case b0 :: b1 :: tail =>
          if ( intersection(day, b0, b1) ) {
            r(b1 :: tail, b0 :: result)
          } else {
            r(b1 :: tail, result)
          }
      }
    }

    List
      .range(0, numOfReductions)
      .foldLeft(buildings){ (acc, _) =>
        r(acc.sortBy(_.initialHeight), Nil)
      }
  }

  def initializeBuildings(input: Input, numOfReductions: Int): List[Building] = {
    reduceNumOfBuildingsByIntersection(
      day = input.maxDay,
      reduceNumOfBuildingsByGrowthRate(
        Building.createBuildings(input)
      ),
      numOfReductions
    )
    .sortBy(_.index)
  }

  @tailrec
  def getHighestBuildingMaxIndex(day: Int, buildings: List[Building], result: Result): Int = {
    buildings match {
      case Nil =>
        result.index
      case h :: tail =>
        val height: Int = h.height(day)
        val nextResult: Result =
          if (height >= result.height && h.index > result.index ) {
            Result(height, h.index)
          } else {
            result
          }
        getHighestBuildingMaxIndex(day, tail, nextResult)
    }
  }

  object model {

    object Building {

      def createBuildings(input: Input): List[Building] = {
        (0 until input.numOfBuildings)
          .map { index =>
            val height = input.initialFloorHeights(index)
            Building(
              index + 1,
              height,
              input.numOfFloorsPerDay(index)
            )
          }.toList
      }

    }

    case class Building(
      index: Int,
      initialHeight: Int,
      growthRate: Int,
    ) {

      def height(day: Int): Int = initialHeight + (day * growthRate)

    }

    object Result {
      def empty: Result = Result(0,0)
    }

    case class Result(height: Int, index: Int)

    object Input {

      private def lineToList(line: String): Array[Int]=
        line
          .split(" ")
          .map(_.toInt)

      def parse(): Input = {
        val nAndQ = lineToList(readLine)
        val n = nAndQ(0)
        val q = nAndQ(1)
        val initialFloorHeights = lineToList(readLine)
        val numOfFloorsPerDay = lineToList(readLine)
        val dayQueries =
          List.range(0, q)
            .map{ _ =>
              readInt
            }.toArray
        Input(n,q, initialFloorHeights,numOfFloorsPerDay, dayQueries)
      }

      def parse(str: String): Input = {
        val lines = str.split("\n")
        val nAndQ = lineToList(lines.head)
        val n = nAndQ(0)
        val q = nAndQ(1)
        val initialFloorHeights = lineToList(lines(1))
        val numOfFloorsPerDay = lineToList(lines(2))
        val dayQueries: Array[Int] =
          (0 until q)
            .map{ index =>
              lines(index + 3).toInt
            }.toArray
        Input(n,q, initialFloorHeights,numOfFloorsPerDay, dayQueries)
      }

    }

    case class Input(
      n: Int,
      q: Int,
      initialFloorHeights: Array[Int],
      numOfFloorsPerDay: Array[Int],
      dayQueries: Array[Int],
    ) {

      def maxDay: Int = dayQueries.max

      def numOfBuildings: Int = n

      def numOfQueries: Int = q

    }

  }

}
