package hackerrank.functionalprogramming.memoizationanddp

import scala.collection.mutable
import scala.math._

object BangaloreBank {

  case class CacheKey(left: Int, right: Int, index: Int)

  def calculateMinimumTime(passwordDigits: IndexedSeq[Int], cache: mutable.HashMap[CacheKey, Int]): Int = {

    def moveAndPressTime(sourceDigit: Int, targetDigit: Int): Int = {
      def resolvedPosition(digit: Int): Int = {
        if ( digit == 0) 10 else digit
      }
      abs(resolvedPosition(sourceDigit) - resolvedPosition(targetDigit)) + 1
    }

    def placeFingers(key: CacheKey, totalTime: Int): Int =
      cache
        .getOrElseUpdate(key, {
          if (key.index >= passwordDigits.length) {
            0
          } else {

            val nextDigit = passwordDigits(key.index)

            min(
              placeFingers(CacheKey(key.left, nextDigit, key.index + 1), moveAndPressTime(key.right, nextDigit)),
              placeFingers(CacheKey(nextDigit, key.right, key.index + 1), moveAndPressTime(key.left, nextDigit))
            )

          }
        }) + totalTime

      (0 to 9)
        .map { digit =>
          placeFingers(CacheKey(passwordDigits.head, digit, 0), 0)
        }.min
  }


  def calculatePasswordInputTimeInSeconds(passwordLine: String): Int = {

    val password = passwordLine.split(" ").map(_.toInt).toIndexedSeq

    val minNumOfSeconds = calculateMinimumTime(password, mutable.HashMap[CacheKey, Int]())

    minNumOfSeconds
  }

}
