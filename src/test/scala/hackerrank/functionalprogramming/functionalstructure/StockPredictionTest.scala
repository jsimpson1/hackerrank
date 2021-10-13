package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.StockPrediction.model.{MinMax, Query, SqrtDecomposition, Indexes}
import hackerrank.functionalprogramming.functionalstructures.data.StockPredictionInputs
import org.scalatest.funsuite.AnyFunSuite


class StockPredictionTest extends AnyFunSuite {


  def sqrtDecompositionTest[I,E](input: I, expected: E)(implicit fn: I => E): Unit = {
    assert((input, fn(input)) == (input, expected))
  }

  test("SqrtDecomposition -- partialBlockCalc") {

    val data = SqrtDecomposition(Array(3,5,2,6,1))

    val actual = data.partialBlockCalc((0 to 2).toList)

    val expected =  Some(MinMax(2,5))

    assert(actual == expected)

    val actualFull = data.partialBlockCalc(data.values.indices.toList)

    val expectedFull =  Some(MinMax(1,6))

    assert(actualFull == expectedFull)
  }


  test("SqrtDecomposition -- queryIndexes") {

    val data = SqrtDecomposition(Array(3,5,2,6,0,7,1,8,10,2))

    implicit def fn: ((Int, Int)) => Option[Indexes] =
      a => data.queryIndexes(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((-1, 9), None)
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 10), None)
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((10, 0), None)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 8), Some(Indexes(Nil, List(0,1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((1, 8), Some(Indexes(List(1,2), List(1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((2, 8), Some(Indexes(List(2), List(1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((3, 8), Some(Indexes(Nil, List(1,2))))

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((4, 8), Some(Indexes(List(4,5), List(2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((5, 8), Some(Indexes(List(5), List(2))))


    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 9), Some(Indexes(List(9), List(0,1,2))))
  }

  test("SqrtDecomposition -- queryIndexes  case 4") {

    val data = SqrtDecomposition(
      "1 2 3 2 1".split(" ").map(_.toInt)
    )

    implicit def fn: ((Int, Int)) => Option[Indexes] =
      a => data.queryIndexes(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((2, 3), Some(Indexes(List(), List(1))))
  }

  test("SqrtDecomposition -- query") {

    val data = SqrtDecomposition(Array(3,5,2,6,0,7,1,8,10,2))

    implicit def fn: ((Int, Int)) => Option[MinMax] =
      a => data.query(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((-1, 9), None)
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 10), None)
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((10, 0), None)

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 9), Some(MinMax(0, 10)))
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 8), Some(MinMax(0, 10)))

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((2, 8), Some(MinMax(0, 10)))

  }

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
