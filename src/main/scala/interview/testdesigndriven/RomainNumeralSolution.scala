package interview.testdesigndriven

object RomainNumeralSolution extends App {

  runTests()

  def integerToRomainNumeralForDigit(i: Int, numerals: String): String = {
    val parts = numerals.toList
    val one: String = parts(0).toString
    val five: String = parts(1).toString
    val ten: String = parts(2).toString
    if (i < 1) {
      ""
    } else if (i < 4) {
      one * i
    } else if (i < 5) {
      one + five
    } else if (i < 9) {
      five + (one * (i - 5))
    } else {
      one + ten
    }
  }

  def romainNumeralRecursive(i: Int, numerals: String): String = {
    if (i == 0) {
      ""
    } else {
      val mulitDigit = romainNumeralRecursive(i / 10, numerals.substring(2))
      val singleDigit = integerToRomainNumeralForDigit(i % 10, numerals)
      mulitDigit + singleDigit
    }
  }

  def integerToRomainNumeral(i: Int): String = {
    romainNumeralRecursive(i, "IVXLCDMV??")
  }

  def test(i: Int, expected: String): Unit = {
    val actual = integerToRomainNumeral(i)
    assertEquals(i.toString, expected, actual)
  }

  def runTests(): Unit = {
    test(-1, "")
    test(0, "")
    test(1, "I")
    test(2, "II")
    test(3, "III")
    test(4, "IV")
    test(5, "V")
    test(6, "VI")
    test(7, "VII")
    test(8, "VIII")
    test(9, "IX")
    test(10, "X")
    test(11, "XI")
    test(12, "XII")
    test(13, "XIII")
    test(14, "XIV")
    test(15, "XV")
    test(16, "XVI")
    test(17, "XVII")
    test(18, "XVIII")
    test(19, "XIX")
    test(20, "XX")
    test(30, "XXX")
    test(40, "XL")
    test(50, "L")
    test(90, "XC");
    test(100, "C");
    test(400, "CD");
    test(500, "D");
    test(900, "CM");
    test(1000, "M");
    test(3999, "MMMCMXCIX");
    test(4000, "MV");
    test(5000, "V");
    // assertEquals("assertEquals success", "a", "a")
    // assertEquals("assertEquals failure", "a", "b")
  }

  def assertEquals(testName: String, expected: String, actual: String): Unit = {
    if (actual == expected) {
      println(s"SUCCESS -- $testName")
    } else {
      println(
        s"""FAILURE -- $testName

                 |  expected=$expected

                 |  actual  =$actual""".stripMargin)
    }
  }


}
