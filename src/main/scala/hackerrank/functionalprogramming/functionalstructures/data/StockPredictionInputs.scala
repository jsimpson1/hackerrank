package hackerrank.functionalprogramming.functionalstructures.data

import scala.io.{BufferedSource, Source}

object StockPredictionInputs {

  val case0 = """5
                |3 5 2 6 1
                |2
                |0 2
                |2 3""".stripMargin

  val case1 = """1
                 |1
                 |2
                 |0 1
                 |0 3""".stripMargin

  val case3 = """5
                |1 1 1 1 1
                |3
                |0 0
                |2 1
                |4 1000000000""".stripMargin

  val case4 = """5
                |1 2 3 2 1
                |7
                |2 100
                |1 0
                |3 1
                |0 0
                |0 1
                |4 4
                |4 1000000000""".stripMargin


  val case5: String = {
    val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/stockPredictionsCase5.txt")
    s.mkString
  }

  lazy val case5Results: List[Int] = {
    val bs: BufferedSource = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/stockPredictionsCase5Result.txt")
    bs
      .mkString
      .split("\n")
      .map(_.toInt)
      .toList
  }

  val case6: String = {
    val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/stockPredictionsCase6.txt")
    s.mkString
  }

  lazy val case6Results: List[Int] = {
    val bs: BufferedSource = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/stockPredictionsCase6Result.txt")
    bs
      .mkString
      .split("\n")
      .map(_.toInt)
      .toList
  }

}
