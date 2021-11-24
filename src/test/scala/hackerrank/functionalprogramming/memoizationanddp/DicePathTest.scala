package hackerrank.functionalprogramming.memoizationanddp

import hackerrank.functionalprogramming.FileTestCase.MemoizationAndDpTestFiles
import hackerrank.functionalprogramming.TestLoggingCapture._
import hackerrank.functionalprogramming.memoizationanddp.DicePath.initialDicePath
import hackerrank.functionalprogramming.memoizationanddp.DicePath.model._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable


class DicePathTest extends AnyFunSuite {


  def initialCache: mutable.HashMap[MnKey, Set[DicePath]] = mutable.HashMap(
    (MnKey(1,1), Set(initialDicePath))
  )

  def sumOfMaximalPathTest(mDown: Int, nRight: Int, expected: Int): Unit = {
    test(s"sumOfMaximalPath $mDown $nRight") {
      val key = MnKey(mDown, nRight)
      val actual: Int = DicePath.sumOfMaximalPath(key, initialCache)
      assert(actual == expected)
    }
  }

  sumOfMaximalPathTest(2,2,9)
  sumOfMaximalPathTest(1,2,4)
  sumOfMaximalPathTest(2,1,6)
  sumOfMaximalPathTest(3,1,12)
  sumOfMaximalPathTest(1,3,10)
  sumOfMaximalPathTest(3,3,19)

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
