package hackerrank.functionalprogramming.memoizationanddp

import hackerrank.functionalprogramming.FileTestCase.MemoizationAndDpTestFiles
import hackerrank.functionalprogramming.TestLoggingCapture._
import org.scalatest.funsuite.AnyFunSuite


class DicePathTest extends AnyFunSuite {


  test("sumOfMaximalPath 2 2") {
    val movesDown = 2
    val movesRight = 2

    val expected = 9
    val actual: Int = DicePath.sumOfMaximalPath(movesDown, movesRight)

    assert(actual == expected)
  }

  test("sumOfMaximalPath 1 2") {

    val down = 1
    val movesRight = 2

    val expected = 4
    val actual: Int = DicePath.sumOfMaximalPath(down, movesRight)

    assert(actual == expected)
  }

  test("sumOfMaximalPath 2 1") {

    val down = 2
    val movesRight = 1

    val expected = 6
    val actual: Int = DicePath.sumOfMaximalPath(down, movesRight)

    assert(actual == expected)
  }

  test("sumOfMaximalPath 3 1") {

    val down = 3
    val movesRight = 1

    val expected = 12
    val actual: Int = DicePath.sumOfMaximalPath(down, movesRight)

    assert(actual == expected)
  }

  test("sumOfMaximalPath 1 3") {

    val down = 1
    val movesRight = 3

    val expected = 10
    val actual: Int = DicePath.sumOfMaximalPath(down, movesRight)

    assert(actual == expected)
  }

  test("sumOfMaximalPath 3 3") {

    val down = 3
    val movesRight = 3

    val expected = 19
    val actual: Int = DicePath.sumOfMaximalPath(down, movesRight)

    assert(actual == expected)
  }

  def stringToInt(s: String): Int = s.toInt

  test("case 0") {

    val inputStr = """4
                     |2 2
                     |1 2
                     |2 1
                     |3 3""".stripMargin


    val expected = List(9, 4, 6, 19)
    val actual: List[Int] =
      captureLogMessages(
        () => DicePath.solveCase(inputStr),
        stringToInt
      )
    assert(actual == expected)
  }

  test("case 3") {

    val inputStr = MemoizationAndDpTestFiles.fileContents("DicePathCase3.txt")

    val expected: List[Int] =
      MemoizationAndDpTestFiles
        .fileContents("DicePathCase3Result.txt")
        .split("\n")
        .map(_.toInt)
        .toList

    val actual: List[Int] =
      captureLogMessages(
        () => DicePath.solveCase(inputStr),
        stringToInt
      )
    assert(actual == expected)

  }

  test("case 6") {

    val inputStr = MemoizationAndDpTestFiles.fileContents("DicePathCase6.txt")

    val expected: List[Int] =
      MemoizationAndDpTestFiles
        .fileContents("DicePathCase6Result.txt")
        .split("\n")
        .map(_.toInt)
        .toList

    val actual: List[Int] =
      captureLogMessages(
        () => DicePath.solveCase(inputStr),
        stringToInt
      )
    assert(actual == expected)

  }


}
