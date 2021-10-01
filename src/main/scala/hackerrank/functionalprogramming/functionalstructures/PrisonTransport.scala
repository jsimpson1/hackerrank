package hackerrank.functionalprogramming.functionalstructures

import scala.annotation.tailrec

object PrisonTransport {

  import model._
  import scala.math._

  def main(args: Array[String]): Unit = {

    val input = """4
                  |2
                  |1 2
                  |1 4""".stripMargin

    println(calcCost(input))

  }

  def calcCost(input: String): Int = {
    val v = parseInput(input)
    calcCost(v._1, v._2)
  }

  def calcBuses(input: String): IndexedSeq[Bus] = {
    val v = parseInput(input)
    calcBuses(v._1, v._2)
  }


  def parseInput(input: String): (Int, IndexedSeq[PrisonerPair]) = {

    val lines = input.split("\n").toIndexedSeq
    val numOfPrisoners = lines.head.toInt
    val numOfPairs = lines(1).toInt

    (
      numOfPrisoners,
      (2 until (numOfPairs + 2))
        .map { i =>
          val input = lines(i).split(" ").map(_.toInt)
          PrisonerPair(Prisoner(input(0)), Prisoner(input(1)))
        }
    )
  }


  def calcCost(numOfPrisoners: Int, prisonerPairs: IndexedSeq[PrisonerPair]): Int = {
    calcBuses(numOfPrisoners, prisonerPairs)
      .map(_.cost)
      .sum
  }


  def calcBuses(numOfPrisoners: Int, prisonerPairs: IndexedSeq[PrisonerPair]): IndexedSeq[Bus] = {

    val chainGangs: IndexedSeq[ChainGang] = establishChainGangs(numOfPrisoners, prisonerPairs)

    val unpairedPrisoners: Int = numOfPrisoners - chainGangs.map(_.prisoners.size).sum

    val unpairedPrisonersSet: IndexedSeq[Int] = (1 to unpairedPrisoners).flatMap(_ => IndexedSeq(1))

    val prisonerGroupSizes: IndexedSeq[Int] = chainGangs.map(_.prisoners.size) ++ unpairedPrisonersSet

    figureOutBusArrangement(prisonerGroupSizes)
  }

  def figureOutBusArrangement(prisonerGroupSizes: IndexedSeq[Int]): IndexedSeq[Bus] = {
    @tailrec
    def r(prisonerGroupSizes: IndexedSeq[Int], result: IndexedSeq[Bus]): IndexedSeq[Bus] = {
      prisonerGroupSizes match {
        case Seq() =>
          result
        case h +: tail =>
          val neededCapacity: Int = pow(ceil(sqrt(h.toDouble)), 2).toInt
          r(tail, Bus(neededCapacity) +: result)
      }
    }
    r(prisonerGroupSizes, IndexedSeq())
  }

  def establishChainGangs(numOfPrisoners: Int, prisonerPairs: IndexedSeq[PrisonerPair]): IndexedSeq[ChainGang] = {

    val allPrisoners: Set[Prisoner] = prisonerPairs.flatMap(_.prisonerSet).toSet

    val findUnion = new MutableFindUnion(numOfPrisoners + 1)

    prisonerPairs
      .foreach { pp =>
        findUnion.union(pp.pOne.number, pp.pTwo.number)
      }


    @tailrec
    def r(prisoners: Set[Prisoner] , result: Map[Int, IndexedSeq[Prisoner]]): IndexedSeq[ChainGang] = {
      prisoners
        .headOption match {
          case None =>
            result
              .values
              .map(value =>
                ChainGang(value.toSet)
              ).toIndexedSeq
          case Some(prisoner) =>
            val parent: Int = findUnion.findParent(prisoner.number)
            result
              .get(parent) match {
                case None =>
                  r(prisoners.tail, result.+((parent, IndexedSeq(prisoner))))
                case Some(prisonerGroup) =>
                  r(prisoners.tail, result.+((parent, prisoner +: prisonerGroup)))
              }
        }
    }

    r(allPrisoners, Map())

  }

  object model {

    case class Bus(capacity: Int) extends Ordered[Bus] {
      def cost: Int = sqrt(capacity).toInt

      override def compare(that: Bus): Int = capacity.compare(that.capacity)
    }

    case class ChainGang(prisoners: Set[Prisoner])

    case class Prisoner(number: Int)

    case class PrisonerPair(pOne: Prisoner, pTwo: Prisoner) {
      def prisonerSet: Set[Prisoner] = Set(pOne, pTwo)
      def asChainGang: ChainGang = ChainGang(prisonerSet)
    }

  }


}
