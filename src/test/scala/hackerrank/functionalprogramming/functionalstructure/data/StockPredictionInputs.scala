package hackerrank.functionalprogramming.functionalstructure.data


object StockPredictionInputs {

  import hackerrank.functionalprogramming.FileTestCase.FunctionalStructureTestFiles._

  val case0 =
    """5
      |3 5 2 6 1
      |2
      |0 2
      |2 3""".stripMargin

  val case1 =
    """1
      |1
      |2
      |0 1
      |0 3""".stripMargin

  val case3 =
    """5
      |1 1 1 1 1
      |3
      |0 0
      |2 1
      |4 1000000000""".stripMargin

  val case4 =
    """5
      |1 2 3 2 1
      |7
      |2 100
      |1 0
      |3 1
      |0 0
      |0 1
      |4 4
      |4 1000000000""".stripMargin


  def caseInputFileName(caseNum: Int): String = s"stockPredictionsCase${caseNum}.txt"

  def caseResultFileName(caseNum: Int): String = s"stockPredictionsCase${caseNum}Result.txt"

  val case5: String = fileContents(caseInputFileName(5))

  lazy val case5Results: List[Int] = {
    fileContents(caseResultFileName(5))
      .split("\n")
      .map(_.toInt)
      .toList
  }

  val case6: String = fileContents(caseInputFileName(6))

  lazy val case6Results: List[Int] = {
    fileContents(caseResultFileName(6))
      .split("\n")
      .map(_.toInt)
      .toList
  }

  val case11: String = fileContents(caseInputFileName(11))

  lazy val case11Results: List[Int] = {
    fileContents(caseResultFileName(11))
      .split("\n")
      .map(_.toInt)
      .toList
  }

}
