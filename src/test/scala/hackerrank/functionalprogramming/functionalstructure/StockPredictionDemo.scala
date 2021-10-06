package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.StockPrediction
import hackerrank.functionalprogramming.functionalstructures.data.StockPredictionInputs

object StockPredictionDemo  {


  def main(args: Array[String]): Unit = {


//    runLengthEncodeValuesDemo()
//    case5()
    caes5Index()
  }


  def caes5Index(): Unit = {
    val inputStr = StockPredictionInputs.case5

    val input = StockPrediction.model.Input.parse(inputStr)


    println(input.prices.take(100).toList)
  }

}
