package hackerrank.functionalprogramming.memoizationanddp

object PentagonalNumbers {

  import model._

  def main(args: Array[String]): Unit = {

    val case0 = """5
                  |1
                  |2
                  |3
                  |4
                  |5""".stripMargin

    val input = Input.parseInput(case0)

    println(s"input=$input")

    input
      .pentagonalNumber
      .foreach( num =>
        println(
          calculatePentagonalNumber(num)
        )
      )

  }

  def calculatePentagonalNumber(n: Long): Long =
    (3*n*n - n)/2

  object model {

    object Input {

      def parseInput(str: String): Input ={
        str
          .split("\n")
          .map(_.toInt)
          .toList match {
            case h :: tail =>
              Input(
                h,
                tail.toArray
              )
            case _ =>
              Input(-1, Array())
          }



      }

    }

    case class Input(
      numOfCases: Int,
      pentagonalNumber: Array[Int]
    )

  }

}
