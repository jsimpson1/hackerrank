package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.StockPrediction.model.MinMax
import hackerrank.functionalprogramming.functionalstructures.data.StockPredictionInputs
import hackerrank.functionalprogramming.functionalstructures.model.{Indexes, SqrtDecomposition}
import org.scalatest.funsuite.AnyFunSuite


class StockPredictionTest extends AnyFunSuite {

  import hackerrank.functionalprogramming.functionalstructures.StockPrediction.minMaxValue

  test("StockPrediction -- calcLengthOfSubArray: case 0 - query1") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(0))

    assert(actual == 2)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 0 - query2") {

    val inputStr = StockPredictionInputs.case0

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(1))

    assert(actual == 3)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 1 - query1") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(0))

    assert(actual == 1)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 1 - query2") {

    val inputStr = StockPredictionInputs.case1

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(1))

    assert(actual == 1)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 3 - query1") {

    val inputStr = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(0))

    assert(actual == 5)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 3") {
    val inputStr: String = StockPredictionInputs.case3

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = """5
                           |5
                           |5""".stripMargin
      .split("\n")
      .map(_.toInt).toList

    val prices = SqrtDecomposition(input.prices)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(prices, query)
        val expected = expectedValues(index)
        println(s"query index=$index")
        assert(actual == expected)
      }
  }

  test("StockPrediction -- calcLengthOfSubArray: case 4 query 3") {

    val inputStr = StockPredictionInputs.case4

    val input = StockPrediction.model.Input.parse(inputStr)

    val actual = StockPrediction.calcLengthOfSubArray(SqrtDecomposition(input.prices), input.queries(2))

    assert(actual == 3)

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

    val data = SqrtDecomposition(input.prices)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual: Int = StockPrediction.calcLengthOfSubArray(data, query)
        val expected = expectedValues(index)
        println(s"index=$index")
        assert(actual == expected)
      }
  }

//  test("StockPrediction -- calcLengthOfSubArray: case 5 - query0 shortened") {
//
//    // actual data length: 43976
//
//    // lengh: 40
//    // value: 117330096, index: 30
//    val data: Array[Int] = {
////      Array.fill(35470 - 30)(1) ++
//      StockPredictionInputs.case5QueryOneShortened.split(" ").map(_.toInt)
////      Array.fill(8506 - 10)(1)
//    }
//
//    /*
//     * value: 117330096
//     * indexOf: 35470
//     */
//    val query = Query(32, 825842066)
//
//    val data = SqrtDecomposition(data)
//
//    val actual = StockPrediction.calcLengthOfSubArray(data, query)
//
//    assert(actual == 8)
//
//  }

  test("StockPrediction -- calcLengthOfSubArray: case 5 - query0") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val queryNum = 0

    val expected = StockPredictionInputs.case5Results(queryNum)

    val query = input.queries(queryNum)

    val data = SqrtDecomposition(input.prices)

    val actual = StockPrediction.calcLengthOfSubArray(data, query)

    assert(actual == expected)

  }

  test("StockPrediction -- calcLengthOfSubArray: case 5") {

    val inputStr: String = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case5Results

    val prices = SqrtDecomposition(input.prices)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(prices, query)
        val expected = expectedValues(index)
        println(s"index=$index/${expectedValues.length - 1}")
        assert(actual == expected)
      }

  }

  test("StockPrediction -- calcLengthOfSubArray: case 6") {

    val inputStr: String = StockPredictionInputs.case6

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case6Results

    val prices = SqrtDecomposition(input.prices)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(prices, query)
        val expected = expectedValues(index)
//        println(s"index=$index")
        assert(actual == expected)
      }

  }

  test("StockPrediction -- calcLengthOfSubArray: case 11") {

    val inputStr: String = StockPredictionInputs.case11

    val input = StockPrediction.model.Input.parse(inputStr)

    val expectedValues = StockPredictionInputs.case11Results

    val prices = SqrtDecomposition(input.prices)

    input
      .queries
      .zipWithIndex
      .foreach { case (query, index) =>
        val actual = StockPrediction.calcLengthOfSubArray(prices, query)
        val expected = expectedValues(index)
        //        println(s"index=$index")
        assert(actual == expected)
      }

  }


}
