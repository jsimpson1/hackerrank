package hackerrank.functionalprogramming.recursion

import java.time.LocalDate

object Day26NestedLogic {

  def main(args: Array[String]): Unit = {

    val line0 = "9 6 2015"
    val line1 = "6 6 2015 "

    val dates = parseTransactionDates(line0, line1)
    println(s"inputDates=${dates}")
    val fine = determineFine(dates)
    println(s"fine=${fine}")
  }

  case class LibraryTransaction(returnDate: LocalDate, dueDate: LocalDate)

  def parseTransactionDates(line0: String, line1: String): LibraryTransaction = try {
    LibraryTransaction(
      parse(line0),
      parse(line1)
    )
  } catch {
    case e: Exception =>
      println(s"parseInput -- error parsing: line0=${line0} and line1=${line1}")
      throw e
  }


  def parse(s: String): LocalDate = {
    val parts = s.split(" ").map(_.toInt)
    parts match {
      case Array(day, month, year) =>
        LocalDate.of(year, month, day)
      case other =>
        throw new RuntimeException(s"unable to parse case ${other.toList}")
    }
  }

  def determineFine(transaction: LibraryTransaction): Int = {

    val hackoFinePerDay = 15
    val hackoFinePerMonth = 500
    val hackoFixedYearFine = 10000

    val dueDate = transaction.dueDate
    val returnDate = transaction.returnDate

    if (returnDate.isBefore(dueDate) || returnDate.equals(dueDate)) {
      0
    } else if (returnDate.getYear > dueDate.getYear) {
      hackoFixedYearFine
    } else if (returnDate.getMonthValue > dueDate.getMonthValue) {
      hackoFinePerMonth * (returnDate.getMonthValue - dueDate.getMonthValue)
    } else if (returnDate.getDayOfMonth > dueDate.getDayOfMonth) {
      hackoFinePerDay * (returnDate.getDayOfMonth - dueDate.getDayOfMonth)
    } else {
      -1
    }
  }

}
