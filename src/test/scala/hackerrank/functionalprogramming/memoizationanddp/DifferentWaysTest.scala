package hackerrank.functionalprogramming.memoizationanddp

import hackerrank.functionalprogramming.FileTestCase.MemoizationAndDpTestFiles
import org.scalatest.funsuite.AnyFunSuite

class DifferentWaysTest extends AnyFunSuite{

  def runTest(poolSize: Int, groupSize: Int, expected: Int): Unit = {
    test(s"calcCombinationPermutationModule $poolSize $groupSize") {
      val actual = DifferentWays.calcCombinationPermutationModule(poolSize, groupSize)
      assert(actual == expected)
    }
  }

//  test("calcCombinationPermutationModule 43 22"){
//    val poolSize = 43
//    val groupSize = 22
//    val expected = 16958725
//    val actual = DifferentWays.calcCombinationPermutationModule(poolSize, groupSize)
//    assert(actual == expected)
//  }
//
//  test("calcCombinationPermutationModule 2 2"){
//    val poolSize = 2
//    val groupSize = 2
//    val expected = 2
//    val actual = DifferentWays.calcCombinationPermutationModule(poolSize, groupSize)
//    assert(actual == expected)
//  }

  runTest(2, 1, 2)
  runTest(5, 1, 5)
  runTest(5, 2, 10)
  runTest(5, 3, 10)
  runTest(10, 5, 252)

  test("case 0") {

    val inputStr = """5
                     |2 1
                     |5 1
                     |5 2
                     |5 3
                     |10 5""".stripMargin

    val expected: List[Int] = List(2, 5, 10, 10, 252)
    val actual = DifferentWays.solve(inputStr)
    assert(actual == expected)
  }

  test("case 1"){

    val inputStr = MemoizationAndDpTestFiles.fileContents("DifferentWaysCase1.txt")

    val expected =
      MemoizationAndDpTestFiles
        .fileContents("DifferentWaysCase1Result.txt")
        .split("\n")
        .map(s => BigInt(s.toInt))
        .toList

    val actual = DifferentWays.solve(inputStr)
    assert(actual == expected)
  }

  test("case 3"){

    val inputStr = MemoizationAndDpTestFiles.fileContents("DifferentWaysCase3.txt")

    val expected =
      MemoizationAndDpTestFiles
        .fileContents("DifferentWaysCase3Result.txt")
        .split("\n")
        .map(s => BigInt(s.toInt))
        .toList

    val actual = DifferentWays.solve(inputStr)
    assert(actual == expected)
  }

}
