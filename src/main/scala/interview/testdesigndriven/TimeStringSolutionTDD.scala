package interview.testdesigndriven

object TimeStringSolutionTDD extends App {

  runTests()

  def secondsToSumString(totalSeconds: Int): String = {
    val hours =  totalSeconds / 3600
    val minutes = (totalSeconds / 60) % 60
    val seconds = totalSeconds % 60
    hours + ":" + minutes + ":" + seconds
  }

  def timeToSeconds(timeStr: String): Int = {
    val parts = timeStr.split(":")
    val minutes = parts(0).toInt
    val seconds = parts(1).toInt
    val minutesAsSeconds = minutes * 60
    val totalSeconds = minutesAsSeconds + seconds
    totalSeconds
  }

  def listOfTimesAsStringsToSum(input: String): String = {
    val timeStrings = input.split(" ")
    val timesAsSeconds = timeStrings.map(timeToSeconds)
    val totalSeconds = timesAsSeconds.sum
    val sum = secondsToSumString(totalSeconds)
    sum
  }

  def runTests(): Unit = {
    val input = "12:32 34:01 15:23 9:27 55:22 25:56"
    val expected = "2:32:41"

    assertEquals("timeToSeconds case 0", timeToSeconds("1:01").toString, "61")

    assertEquals("solve case 0", listOfTimesAsStringsToSum(input), expected)
  }

  def assertEquals(testname: String, actual: String, expected: String): Unit = {
    if ( actual == expected ) {
      println(s"SUCCESS -- $testname")
    } else {
      println(s"""FAILURE -- $testname
                 |  expected=$expected
                 |  actual  =$actual""".stripMargin)
    }
  }

}
