package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.PrisonTransport
import PrisonTransport.model
import PrisonTransport.model.Bus
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class PrisonTransportTest extends AnyFunSuite {


  test("case 0") {
    val inputStr = """4
                     |2
                     |1 2
                     |1 4""".stripMargin

    val actual: Int = PrisonTransport.calcCost(inputStr)

    assertResult(3)(actual)

  }

  test("case 0 buses") {
    val inputStr = """4
                     |2
                     |1 2
                     |1 4""".stripMargin

    val actual: IndexedSeq[model.Bus] = PrisonTransport.calcBuses(inputStr).sorted

    val expected = IndexedSeq(
      Bus(1),
      Bus(4)
    )

    assertResult(expected)(actual)

  }

  test("case 9") {

    val file = "/Users/flow/code/jeremy/hackerrank/test_cases/prisonerTransportCase9.txt"

    val inputStr = Source.fromFile(file)

    val actual: Int = PrisonTransport.calcCost(inputStr.getLines().mkString("\n"))

    assertResult(19160)(actual)
  }

}
