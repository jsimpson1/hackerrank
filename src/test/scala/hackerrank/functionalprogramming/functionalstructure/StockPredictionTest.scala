package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.StockPrediction.model.Query
import hackerrank.functionalprogramming.functionalstructures.data.StockPredictionInputs
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

class StockPredictionTest extends AnyFunSuite {

  test("case 0 - query1") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, input.queries(0))

    assert(actual == 2)

  }

  test("case 0 - query2") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, input.queries(1))

    assert(actual == 3)

  }

  test("case 1 - query1") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, input.queries(0))

    assert(actual == 1)

  }

  test("case 1 - query2") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, input.queries(1))

    assert(actual == 1)

  }

  test("case 3 - query1") {

    val inputStr = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, input.queries(0))

    assert(actual == 5)

  }

  test("case 3") {
    val inputStr: String = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = """5
                           |5
                           |5""".stripMargin
      .split("\n")
      .map(_.toInt).toList

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(input.prices, query)
        val expected = expectedValues(index)
        println(s"query index=$index")
        assert(actual == expected)
      }
  }

  test("case 4") {
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

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(input.prices, query)
        val expected = expectedValues(index)
        println(s"index=$index")
        assert(actual == expected)
      }
  }

  test("case 5 - query0") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val queryNum = 0

    val expected = StockPredictionInputs.case5Results(queryNum)

    val query = input.queries(queryNum)

    val actual = StockPrediction.calcLengthOfSubArray(input.prices, query)

    assert(actual == expected)

  }


  test("case 5") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case5Results

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(input.prices, query)
        val expected = expectedValues(index)
//        println(s"index=$index")
        assert(actual == expected)
      }

  }

  test("case 6") {

    val inputStr: String = StockPredictionInputs.case6

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case6Results

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(input.prices, query)
        val expected = expectedValues(index)
//        println(s"index=$index")
        assert(actual == expected)
      }

  }


}
