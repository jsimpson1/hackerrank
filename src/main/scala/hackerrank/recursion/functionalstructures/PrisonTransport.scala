package hackerrank.recursion.functionalstructures

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.JavaConverters._

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

    val lines = input.lines.toIndexedSeq
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

    val chainGangs: IndexedSeq[ChainGang] = establishChainGangs(prisonerPairs)

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

  def establishChainGangs(prisonerPairs: IndexedSeq[PrisonerPair]): IndexedSeq[ChainGang] = {
    @tailrec
    def r(prisonerPair: IndexedSeq[PrisonerPair], chainGangs: IndexedSeq[ChainGang]): IndexedSeq[ChainGang] = {
      prisonerPair match {
        case Seq() =>
          chainGangs
        case h +: tail =>
          val existingGangs: IndexedSeq[ChainGang] =
            chainGangs
              .filter(g =>
                g.prisoners.contains(h.pOne) || g.prisoners.contains(h.pTwo)
              )
          val newChainGang: ChainGang =
            existingGangs
             match {
                case IndexedSeq(g0, g1) =>
                  ChainGang(h.prisonerSet ++ g0.prisoners ++ g1.prisoners)
                case IndexedSeq(g0) =>
                  ChainGang(h.prisonerSet ++ g0.prisoners)
                case Seq() =>
                  h.asChainGang
                case _ => throw new RuntimeException("there should never be three chaingangs that share two prisoners")
              }
          val nextChainGangs: IndexedSeq[ChainGang] = newChainGang +: chainGangs.diff(existingGangs)
          r(tail, nextChainGangs)
      }
    }
    r(prisonerPairs, IndexedSeq())
  }

  object model {

    case class Bus(capacity: Int) extends Ordered[Bus] {
      def cost: Int = sqrt(capacity).toInt

      override def compare(that: Bus): Int = capacity.compare(that.capacity)
    }

    case class ChainGang(prisoners: Set[Prisoner], id: String = UUID.randomUUID().toString)

    case class Prisoner(number: Int)

    case class PrisonerPair(pOne: Prisoner, pTwo: Prisoner) {
      def prisonerSet: Set[Prisoner] = Set(pOne, pTwo)
      def asChainGang: ChainGang = ChainGang(prisonerSet)
    }

  }


}
