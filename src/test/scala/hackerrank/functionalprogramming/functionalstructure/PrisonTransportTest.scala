package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.PrisonTransport
import PrisonTransport.model.Bus
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class PrisonTransportTest extends AnyFunSuite {

  import hackerrank.functionalprogramming.FileTestCase.FunctionalStructureTestFiles._

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

    val actual: IndexedSeq[Bus] = PrisonTransport.calcBuses(inputStr).sorted

    val expected = IndexedSeq(
      Bus(1),
      Bus(4)
    )

    assertResult(expected)(actual)

  }

  test("case 9") {

    val inputStr = fileContents("prisonerTransportCase9.txt")

    val actual: Int = PrisonTransport.calcCost(inputStr)

    assertResult(19160)(actual)
  }

}
