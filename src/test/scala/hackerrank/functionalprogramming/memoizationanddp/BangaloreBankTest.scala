package hackerrank.functionalprogramming.memoizationanddp

import hackerrank.functionalprogramming.FileTestCase.MemoizationAndDpTestFiles
import org.scalatest.funsuite.AnyFunSuite

class BangaloreBankTest extends AnyFunSuite {

  test("password: 1 2") {

    val passwordLine = "1 2"

    val expected = 2
    val actual = BangaloreBank.calculatePasswordInputTimeInSeconds(passwordLine)

    assert(actual == expected)
  }

  test("password: 1 0 3") {

    val passwordLine = "1 0 3"

    val expected = 5
    val actual = BangaloreBank.calculatePasswordInputTimeInSeconds(passwordLine)

    assert(actual == expected)
  }

  test("case 2") {

    val inputStr = MemoizationAndDpTestFiles.fileContents("BangaloreBankCase2.txt")

    val expected = 24399
    val actual = BangaloreBank.calculatePasswordInputTimeInSeconds(inputStr.split("\n")(1))

    assert(actual == expected)
  }


}
