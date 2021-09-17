package hackerrank.recursion.functionalstructures

import scala.annotation.tailrec

object GreatestCommonDivisor {


  import model._

  def main(args: Array[String]): Unit = {

    lazy val input = """2
                       |7 2
                       |2 2 7 1""".stripMargin

    val parsedInput = parseInput(input)

    println(s"input=${parsedInput}")

    println(s"output=${calcGreatestCommonDivisor(parsedInput)}")



  }


  def parseInput(str: String): List[List[Divisor]] = {
    val lines = str.split("\n")

    val sizeOfList = lines.head.toInt

    List.range(0, sizeOfList)
      .map{ i =>
        lines
          .apply(i + 1)
          .split(" ")
          .map(_.toInt)
          .sliding(2, 2)
          .map(v => Divisor(v.head, v(1)))
          .toList
      }
  }

  def calcGreatestCommonDivisorResult(str: String): String =
    calcGreatestCommonDivisor(parseInput(str))
      .mkString(" ")

  def calcGreatestCommonDivisor(lst: List[List[Divisor]]): List[Divisor] = {

    @tailrec
    def r(
      smallestDivisor: List[Divisor],
      remainingDivisors: List[List[Divisor]],
      result: List[Divisor],
    ): List[Divisor] = {
      smallestDivisor match {
        case Nil =>
          result.reverse
        case h :: tail =>
          remainingDivisors
            .map(rd =>
              rd.find(_.hasCommonPrime(h))
            ) match {
              case divisors if divisors.forall(_.isDefined) =>
                val minPower = (Some(h) :: divisors).flatten.map(_.power).min
                r(tail, remainingDivisors, Divisor(h.prime, minPower) :: result)
              case _ =>
                r(tail, remainingDivisors, result)
            }
      }
    }

    lst
      .sortBy(_.size) match {
        case h :: Nil =>
          h
        case h :: tail =>
          r(h, tail, Nil)
      }

  }

  object model {

    case class Divisor(prime: Int, power: Int) {

      def hasCommonPrime(d: Divisor): Boolean = d.prime == prime

      override def toString: String = s"$prime $power"

    }

  }

}
