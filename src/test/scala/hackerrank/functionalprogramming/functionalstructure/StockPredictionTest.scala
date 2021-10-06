package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.data.StockPredictionInputs
import org.scalatest.funsuite.AnyFunSuite


class StockPredictionTest extends AnyFunSuite {


  test("runLengthEncoding simple test zero index") {

    val prices = Array(1,2,3,4)

    val startIndex = 0
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(1,2,3,-1), 0, compressionValue)

    assert(actual == expected)

  }

  test("runLengthEncoding all valid") {

    val prices = Array(1,1,1,1)

    val startIndex = 2
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(1,1,1,1), 2, compressionValue)

    assert(actual == expected)
  }

  test("runLengthEncoding empty") {

    val prices: Array[Int] = Array()

    val startIndex = 0
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(), 0, compressionValue)

    assert(actual == expected)
  }

  test("runLengthEncoding simple test") {

    val prices = Array(1,2,3,4,5,6,1,3)

    val startIndex = 2
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(-1,3,4,5,-1,3), 1, compressionValue)

    assert(actual == expected)

  }

  test("runLengthEncoding simple test 2") {

    val prices = Array(1,1,5,1,2,3,4,5,6,1,3)

    val startIndex = 5
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(-1,5,-1,3,4,5,-1,3), 3, compressionValue)

    assert(actual == expected)

  }

  test("runLengthEncoding simple test 3") {

    val prices = Array(1,1,1,1,5,1,2,3,4,5,6,1,3)

    val startIndex = 7
    val margin = 2

    val compressionValue = -1

    val actual = StockPrediction.model.PerProcessedPrices.runLengthEncoding(prices,startIndex, margin, compressionValue)

    val expected = StockPrediction.model.PerProcessedPrices(List(-1,5,-1,3,4,5,-1,3), 3, compressionValue)

    assert(actual == expected)

  }

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
//        println(s"index=$index/${expectedValues.length - 1}")
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

  test("case 11") {

    val inputStr: String = StockPredictionInputs.case11

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case11Results

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
