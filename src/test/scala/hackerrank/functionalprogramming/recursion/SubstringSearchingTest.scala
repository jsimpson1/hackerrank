package hackerrank.functionalprogramming.recursion

import org.scalatest.funsuite.AnyFunSuite

class SubstringSearchingTest extends AnyFunSuite {

  test("case 0 - abcdef") {
    assertResult(true)(SubstringSearching.hasPattern("abcdef", "def"))
  }

  test("case 0 - computer") {
    assertResult(false)(SubstringSearching.hasPattern("computer", "muter"))
  }

  test("case 0 - stringmatchingmat") {
    assertResult(true)(SubstringSearching.hasPattern("stringmatchingmat", "ingmat"))
  }

  test("case 0 - videobox") {
    assertResult(true)(SubstringSearching.hasPattern("videobox", "videobox"))
  }

  val case5FullInput = SubstringSearchInput.case5.split("\n")

  test("case 5 - 0") {
    val text = case5FullInput(1)
    val pattern = case5FullInput(2)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }

  test("case 5 - 1") {
    val text = case5FullInput(3)
    val pattern = case5FullInput(4)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(false)(actual)
  }

  test("case 5 - 2") {
    val text = case5FullInput(5)
    val pattern = case5FullInput(6)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }

  test("case 5 - 3") {
    val text = case5FullInput(7)
    val pattern = case5FullInput(8)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }

  test("case 5 - 3 shortened 0") {
    val text = "eenegpurdzqahowsfhqcibixfzjcozegqguoqntgcouxeaabbdkzcfjmaomblvbutwjuhuvhagqfhtxpgghtlomdjcyhwmmbgswusqplvziqqzkptxtawswqqvjnqjpghhayctjnqgdvwculprcuioawxwbgskvkbvkxsxxkcbbkyskaepzaannhanmrpkpzstzoidmgoxyogiwyybbgshfeacaksyvrmilhkcylenuvokshjdnahddlqaqgzhfvgazxdgxgvferoaqyutuvbvadopgmnjuvesbuljlokiyexszumpgehuiswurhricrtcjbmkvcsbhuyaykkbnkjrqisstokaensjqtheznoxnlzqqyrboalzhxqwzcjcapntqnheyykgaitsghmviupfhqwokieomp"
    val pattern = "enegpurdzqahowsfhqcibixfzjcozegqguoqntgcouxeaabbdkzcfjmaomblvbutwjuhuvhagqfhtxpgghtlomdjcyhwmmbgswusqplvziqqzkptxtawswqqvjnqjpghhayctjnqgdvwculprcuioawxwbgskvkbvkxsxxkcbbkyskaepzaannhanmrpkpzstzoidmgoxyogiwyybbgshfeacaksyvrmilhkcylenuvokshjdnahddlqaqgzhfvga"
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }

  test("case 5 - 4") {
    val text = case5FullInput(9)
    val pattern = case5FullInput(10)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(false)(actual)
  }

  test("case 5 - 5") {
    val text = case5FullInput(11)
    val pattern = case5FullInput(12)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(false)(actual)
  }

  test("case 5 - 6") {
    val text = case5FullInput(13)
    val pattern = case5FullInput(14)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(false)(actual)
  }

  test("case 5 - 7") {
    val text = case5FullInput(15)
    val pattern = case5FullInput(16)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(false)(actual)
  }

  test("case 5 - 8") {
    val text = case5FullInput(17)
    val pattern = case5FullInput(18)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }

  test("case 5 - 9") {
    val text = case5FullInput(19)
    val pattern = case5FullInput(20)
    val actual = SubstringSearching.hasPattern(text, pattern)
    assertResult(true)(actual)
  }


}
