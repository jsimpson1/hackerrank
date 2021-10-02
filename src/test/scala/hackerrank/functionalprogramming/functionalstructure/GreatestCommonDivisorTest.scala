package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.GreatestCommonDivisor
import org.scalatest.funsuite.AnyFunSuite

class GreatestCommonDivisorTest extends AnyFunSuite{

  import GreatestCommonDivisor._
  import GreatestCommonDivisor.model._

  def doTest(testName: String, input: String, expected: List[Divisor]): Unit = {
    val pInput = parseInput(input)
    val actual: List[Divisor] = calcGreatestCommonDivisor(pInput)
    println(
      s"""test: $testName
         |expected=$expected
         |actual  =$actual""".stripMargin)

    assertResult(expected)(actual)

  }

  test("case 0") {

    val input = """2
                   |7 2
                   |2 2 7 1""".stripMargin

    doTest("case 0", input, List(Divisor(7,1)))

  }

  test("case 1") {
    val input = """4
                             |2 2 3 2 5 3
                             |3 2 5 3 11 1
                             |2 2 3 3 5 4 7 6 19 18
                             |3 10 5 15""".stripMargin

    doTest("case 1", input, List(Divisor(3,2), Divisor(5,3)))
  }

  test("case 2") {
    val input = """2
                  |2 2 3 1 5 3 29 1
                  |2 3 5 3 11 1 31 1""".stripMargin

    doTest("case 2", input, List(Divisor(2,2), Divisor(5,3)))
  }

}
