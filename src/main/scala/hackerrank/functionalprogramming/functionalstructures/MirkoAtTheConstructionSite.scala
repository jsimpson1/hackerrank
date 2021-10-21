package hackerrank.functionalprogramming.functionalstructures

import scala.collection.immutable.HashMap

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

  def solve(input: Input): List[Int] = {
    val maxDay = input.dayQueries.max

    val buildings: Array[Building] = Building.createBuildings(input)

    val dayToBuildings: HashMap[Int, Array[Building]] = dayToBuildingsMap(maxDay, buildings)

    input
      .dayQueries
      .map{ day =>
        val buildingsOfTheDay: Array[Building] = dayToBuildings
          .getOrElse(day, throw new Throwable(s"day=$day not found in dayToBuildings"))

        val maxHeight: Int =
          buildingsOfTheDay
            .map(_.currentHeight)
            .max

        buildingsOfTheDay
          .filter(_.currentHeight == maxHeight)
          .map(_.index).max

      }
      .toList
  }

  def dayToBuildingsMap(maxDay: Int, buildings: Array[Building]): HashMap[Int, Array[Building]] = {
    (0 to maxDay)
      .foldLeft(HashMap[Int, Array[Building]]()) { (map: HashMap[Int, Array[Building]] , dayNum: Int) =>
        val nextBuildings: Array[Building] = {
          buildings
            .map { building =>
              building
                .copy(
                  currentHeight = building.initialHeight + (dayNum * building.dailyFloorIncrement)
                )
            }

        }
        map.+((dayNum, nextBuildings))
      }
  }

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

      def createBuildings(input: Input): Array[Building] = {
        (0 until input.numOfBuildings)
          .map { index =>
            val height = input.initialFloorHeights(index)
            Building(
              index + 1,
              height,
              height,
              input.numOfFloorsPerDay(index)
            )
          }.toArray
      }

    }

    case class Building(
      index: Int,
      initialHeight: Int,
      currentHeight: Int,
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
