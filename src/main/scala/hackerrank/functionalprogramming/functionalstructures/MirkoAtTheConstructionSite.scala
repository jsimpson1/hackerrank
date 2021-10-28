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

  def solve(str: String): List[Int] =
    solve(Input.parse(str))

  def printStepsWithTime(context: String): Unit = {

    import java.time._

    println(s"solve -- ${LocalDateTime.now()} $context")
  }

  def solve(input: Input): List[Int] = {


    val maxDay = input.dayQueries.max

    val buildings: List[Building] =
      Building
        .createBuildings(input)
        .groupBy(_.dailyFloorIncrement)
        .map { case (_, buildings) =>
          buildings
            .maxBy(_.initialHeight)
        }
        .toList
        .sortBy(_.index)


    val dayToIndex: TreeMap[Int, Int] = dayToMaxHeightIndex(maxDay, buildings)

    input
      .dayQueries
      .map{ day =>
        dayToIndex
          .getOrElse(day, throw new Throwable(s"day=$day not found in dayToBuildings"))
      }
      .toList
  }

  case class Result(height: Int, index: Int)

  @tailrec
  def getHighestBuildingMaxIndex(numOfDays: Int, buildings: List[Building], result: Result): Int = {
    buildings match {
      case Nil =>
        result.index
      case h :: tail =>
        val height: Int = h.initialHeight + (numOfDays * h.dailyFloorIncrement)
        val nextResult: Result =
          if (height >= result.height && h.index > result.index ) {
            Result(height, h.index)
          } else {
            result
          }
        getHighestBuildingMaxIndex(numOfDays, tail, nextResult)
    }
  }

  def dayToMaxHeightIndex(maxDay: Int, buildings: List[Building]): TreeMap[Int, Int] = {

    val lastDayMaxIndex: Int = getHighestBuildingMaxIndex(maxDay, buildings, Result(0, 0))

    (0 to maxDay)
      .foldLeft(
        DayToMaxHeightIndexTemp(
          TreeMap[Int, Int](),
          doShortcut = false
        )
      ){ (result, dayNum) =>

        if ( result.doShortcut ) {
          result
            .copy(
              map = result.map.+((dayNum, lastDayMaxIndex))
            )
        } else {
          val maxIndex = getHighestBuildingMaxIndex(dayNum, buildings, Result(0, 0))
          if ( maxIndex == lastDayMaxIndex ) {
            result.copy(
              map = result.map.+((dayNum, maxIndex)),
              doShortcut = true
            )
          } else {
            result.copy(
              map = result.map.+((dayNum, maxIndex)),
            )
          }
        }
      }.map
  }

  case class DayToMaxHeightIndexTemp(
    map: TreeMap[Int, Int],
    doShortcut: Boolean
  )

  object model {

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
      dailyFloorIncrement: Int,
    )

    case class Input(
      n: Int,
      q: Int,
      initialFloorHeights: Array[Int],
      numOfFloorsPerDay: Array[Int],
      dayQueries: Array[Int],
    ) {

      def numOfBuildings: Int = n

      def numOfQueries: Int = q

    }

  }

}
