package hackerrank.functionalprogramming

import java.io.ByteArrayOutputStream

object TestLoggingCapture {

  private def streamToNodeValues[A](stream: ByteArrayOutputStream, stringToValueFunction: (String) => A): List[A] = {
    val str = stream.toString
    str match {
      case "" => Nil
      case s =>
        s
          .split("\n")
          .toList
          .map(s =>
            stringToValueFunction(s)
          )
    }

  }

  def stringToInt(s: String): Int = s.toInt

  def captureLogMessages[A](functionToTest: () => Unit, stringToValueFunction: String => A): List[A] = {
    val stream = new ByteArrayOutputStream()
      Console
        .withOut(stream) {
          functionToTest()
        }
    streamToNodeValues(stream, stringToValueFunction)
  }

}
