package hackerrank.functionalprogramming.functionalstructures


import scala.collection.immutable.HashMap

object FightingArmies {

  import model._
  import scala.io.StdIn._

  def main(args: Array[String]): Unit = {

    val n = 10

    val command = Command.apply(n)

    command.printArmies()

    (0 to n).foreach { i =>
      command.recruit(i, i + 1)
    }

    command.printArmies()

    command.mergeArmies(0, 1)

    command.printArmies()

    command.strongestDied(0)

    command.printArmies()

    command.strongestDied(0)

    command.printArmies()

    command.strongestDied(0)

    command.printArmies("testing after empty array kill strongest")

  }

  def solve(): List[Int] = {
    val nAndq = readLine.split(" ").map(_.toInt)
    val n: Int = nAndq(0)
    val q: Int = nAndq(1)
    val command = Command.apply(n)
    List.range(0, q).flatMap { _ =>
      val event = readLine.split(" ").map(_.toInt)
      Event.processEvent(command, event)
    }
  }

  def solve(input: Input): List[Int] = {
    val command = Command.apply(input.numOfArmies)
    applyEvents(command, input.events)
  }

  private def applyEvents(command: Command, events: List[Array[Int]]): List[Int] = {
    events
      .flatMap( event =>
        Event.processEvent(command, event)
      )
  }

  object model {

    object Event {

      def processEvent(command: Command, str: String): Option[Int] =
        processEvent(command, str.split(" ").map(_.toInt))

      def processEvent(command: Command, event: Array[Int]): Option[Int] = {
        event match {
          case Array(1, i) =>
            Some(command.findStrongest(i))
          case Array(2, i) =>
            command.strongestDied(i)
            None
          case Array(3, i, c) =>
            command.recruit(i, c)
            None
          case Array(4, i, j) =>
            command.mergeArmies(i, j)
            None
        }
      }

    }

    object Input {

      def parseInput(inputString: String): Input = {
        inputString
          .split("\n").toList match {
            case h :: tail =>
              val nAndq  = h.split(" ").map(_.toInt)
              Input(
                nAndq(0),
                nAndq(1),
                tail.map(eventLine =>
                  eventLine
                    .split(" ")
                    .map(_.toInt)
                )
              )
          }
      }
    }

    case class Input(numOfArmies: Int, numOfEvents: Int, events: List[Array[Int]])

    object Command {

      def apply(n: Int): Command = {
        val map: HashMap[Int, Array[Int]] = HashMap[Int, Array[Int]]() ++ (
          (0 to n)
            .toArray
            .map(index =>
              (index, Array[Int]())
            )
          )
        Command(map)
      }

    }

    case class Command(var armies: HashMap[Int, Array[Int]]) {

      def printArmies(context: String = ""): Unit = {
        lazy val contextMessage =
          if (context.nonEmpty)
            s"context: $context\n"
          else
            ""
        val armiesMessage = armies.map{ case (index, army) => (index, army.toList) }.toString()
        println(
          s"${contextMessage}  armies : $armiesMessage"
        )
      }

      private def getArmy(i: Int): Array[Int] =
        armies
          .getOrElse(i,
            throw new Throwable(s"getArmy -- i=${i} not found")
          )

      def findStrongest(i: Int): Int = {
        val strongest = getArmy(i).max
        println(strongest)
        strongest
      }

      def strongestDied(i: Int): Unit = {
        getArmy(i) match {
          case Array() =>
            println(s"strongestDied -- i=$i. No soldiers in army so unable to kill the strongest")
          case army =>
            armies = armies.updated(i, army.patch(army.indexWhere(_ == army.max), Nil, 1))
        }
      }

      def recruit(i: Int, c: Int): Unit = {
        armies = armies.updated(i, getArmy(i).+:(c))
      }

      def mergeArmies(i: Int, j: Int): Unit = {
        armies = armies.updated(i, getArmy(i) ++ getArmy(j)) - j
      }

    }

  }

}
