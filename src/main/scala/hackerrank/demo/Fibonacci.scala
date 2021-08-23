package hackerrank.demo

import scala.annotation.tailrec
import scala.math.pow

object Fibonacci {

  def apply(n: Long, mod: Long): Long = {
    @tailrec
    def r(i: Long, a: Long, b: Long): Long = {
      i match {
        case 0 => a
        case _ => r(i-1, b, (a+b) % mod)
      }
    }
    r(n, 0, 1)
  }

}
