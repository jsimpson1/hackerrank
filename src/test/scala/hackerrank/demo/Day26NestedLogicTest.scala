package hackerrank.demo

import hackerrank.functionalprogramming.demo.Day26NestedLogic._
import org.scalatest.funsuite.AnyFunSuite

class Day26NestedLogicTest extends AnyFunSuite {

  // input: "day month year"

  test("case0") {
    val returnDate = "9 6 2015"
    val dueDate = "6 6 2015"
    val fine = determineFine(parseTransactionDates(returnDate, dueDate))
    val expected = 45
    assertResult(expected)(fine)
  }

  test("case1") {
    val returnDate = "1 1 2015"
    val dueDate = "31 12 2014"
    val fine = determineFine(parseTransactionDates(returnDate, dueDate))
    val expected = 10000
    assertResult(expected)(fine)
  }

  test("case3") {
    val returnDate = "31 12 2009"
    val dueDate = "1 1 2010"
    val fine = determineFine(parseTransactionDates(returnDate, dueDate))
    val expected = 0
    assertResult(expected)(fine)
  }

  test("same day") {
    val returnDate = "31 12 2009"
    val dueDate = returnDate
    val fine = determineFine(parseTransactionDates(returnDate, dueDate))
    val expected = 0
    assertResult(expected)(fine)
  }

}
