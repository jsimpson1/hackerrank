package hackerrank.recursion.functionalstructures

object MatrixRotationInputs {

  val case0 = """4 4 1
                |1 2 3 4
                |5 6 7 8
                |9 10 11 12
                |13 14 15 16""".stripMargin

  val case0Result = """2 3 4 8
                      |1 7 11 12
                      |5 6 10 16
                      |9 13 14 15""".stripMargin

  val case1 = """4 4 2
                |1 2 3 4
                |5 6 7 8
                |9 10 11 12
                |13 14 15 16""".stripMargin

  val case1Result = """3 4 8 12
                      |2 11 10 16
                      |1 7 6 15
                      |5 9 13 14""".stripMargin

  val case2 = """5 4 7
                |1 2 3 4
                |7 8 9 10
                |13 14 15 16
                |19 20 21 22
                |25 26 27 28""".stripMargin

  val case2Result = """28 27 26 25
                      |22 9 15 19
                      |16 8 21 13
                      |10 14 20 7
                      |4 3 2 1""".stripMargin

  val case3 = """2 2 3
                |1 1
                |1 1""".stripMargin

  val case3Result = """1 1
                      |1 1""".stripMargin

}
