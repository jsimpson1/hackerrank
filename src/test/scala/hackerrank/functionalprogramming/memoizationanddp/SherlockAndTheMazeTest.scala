package hackerrank.functionalprogramming.memoizationanddp

import hackerrank.functionalprogramming.FileTestCase.MemoizationAndDpTestFiles
import hackerrank.functionalprogramming.TestLoggingCapture._
import hackerrank.functionalprogramming.memoizationanddp.SherlockAndTheMaze.{GridSquare, Path, PathKey}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class SherlockAndTheMazeTest extends AnyFunSuite {

//  val cache: mutable.HashMap[PathKey, Set[Path]] =
//    mutable.HashMap[PathKey, Set[Path]](
//      (PathKey(1,1,0), Set(Path(List(GridSquare(1,1)), 0))),
//    )

  test("1 1 0") {

    val inputStr = "1 1 0"

    val expected = 1
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("4 1 0") {
    val inputStr = "4 1 0"

    val expected = 1
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("1 4 0") {
    val inputStr = "1 4 0"

    val expected = 1
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("2 2 3") {

    val inputStr = "2 2 3"

    val expected = 2
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("2 3 1") {

    val inputStr = "2 3 1"

    val expected = 2
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("4 4 4") {

    val inputStr = "4 4 4"

    val expected = 18
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("2 38 27") {

    val inputStr = "2 38 27"

    val expected = 38
    val actual = SherlockAndTheMaze.solveLine(inputStr)

    assert(actual == expected)
  }

  test("case 0") {

    val inputStr = """3
                     |2 2 3
                     |2 3 1
                     |4 4 4""".stripMargin

    val expected = List(2,2,18)

    val actual: List[Int] = captureLogMessages(
      () => SherlockAndTheMaze.solve(inputStr),
      stringToInt
    )
    assert(actual == expected)
  }

  test("case 1") {

    val inputStr = MemoizationAndDpTestFiles.fileContents("SherlockAndTheMazeCase1.txt")

    val expected =
      MemoizationAndDpTestFiles
        .fileContents("SherlockAndTheMazeCase1Result.txt").split("\n")
        .map(_.toInt)
        .toList

    val actual: List[Int] = captureLogMessages(
      () => SherlockAndTheMaze.solve(inputStr),
      stringToInt
    )
    assert(actual == expected)
  }

}
