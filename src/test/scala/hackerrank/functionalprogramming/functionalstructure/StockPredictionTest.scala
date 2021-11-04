package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructure.data.StockPredictionInputs
import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.StockPrediction.model._
import org.scalatest.funsuite.AnyFunSuite


class StockPredictionTest extends AnyFunSuite {


  val printIndexes = false

  def calculateActual(input: Input, queryIndex: Int): Int = {
    val blocks = StockPrediction.createBlocks(input)

    val query = input.queries(queryIndex)

    StockPrediction.solveQuery(query, input.prices, blocks)

  }

  def solveCase(input: Input, expectedValues: List[Int]): Unit = {
    val prices = input.prices

    val blocks = StockPrediction.createBlocks(input)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.solveQuery(query, prices, blocks)
        val expected = expectedValues(index)

        if ( printIndexes )
          println(s"index=$index/${expectedValues.length - 1}")

        assert(actual == expected)
      }
  }

  test("StockPrediction -- calcLengthOfSubArray: case 0 - query 0") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 0)

    assert(actual == 2)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 0 - query 1") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 1)

    assert(actual == 3)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 1 - query 0") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 0)

    assert(actual == 1)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 1 - query  1") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 1)

    assert(actual == 1)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 3 - query 0") {

    val inputStr = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 0)

    assert(actual == 5)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 4 query 2") {

    val inputStr = StockPredictionInputs.case4

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = calculateActual(input, 2)

    assert(actual == 3)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 5 - query 0") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val queryNum = 0

    val actual = calculateActual(input, queryNum)

    val expected = StockPredictionInputs.case5Results(queryNum)

    assert(actual == expected)

  }

  // Full cases ----------------------------------------------------------------------------------------

  test("StockPrediction -- calcLengthOfSubArray: case 3") {
    val inputStr: String = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = """5
                           |5
                           |5""".stripMargin
      .split("\n")
      .map(_.toInt).toList

    solveCase(input, expectedValues)
  }



  test("StockPrediction -- calcLengthOfSubArray: case 4") {
    val inputStr: String = StockPredictionInputs.case4

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = """1
                           |1
                           |3
                           |1
                           |2
                           |5
                           |5""".stripMargin
      .split("\n")
      .map(_.toInt).toList

    solveCase(input, expectedValues)
  }

  test("StockPrediction -- calcLengthOfSubArray: case 5") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case5Results

    solveCase(input, expectedValues)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 6") {

    val inputStr: String = StockPredictionInputs.case6

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case6Results

    solveCase(input, expectedValues)

  }



  test("StockPrediction -- calcLengthOfSubArray: case 11") {

    val inputStr: String = StockPredictionInputs.case11

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues: List[Int] = StockPredictionInputs.case11Results

    solveCase(input, expectedValues)

  }


}
