package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.FightingArmies
import org.scalatest.funsuite.AnyFunSuite

class FightingArmiesTest extends AnyFunSuite {

  import FightingArmies.model._

  test("case 0") {
    val inputStr = """2 6
                  |3 1 10
                  |3 2 20
                  |4 1 2
                  |1 1
                  |2 1
                  |1 1""".stripMargin

    val input = Input.parseInput(inputStr)

    val actual = FightingArmies.solve(input)

    val expected = List(20, 10)

    assert(actual == expected)
  }

}
