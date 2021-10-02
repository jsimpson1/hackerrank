package hackerrank.functionalprogramming.recursion

import scala.annotation.tailrec

object Fibonacci {

  def apply(n: Long, mod: Long): Long = {
    @tailrec
    def r(i: Long, a: Long, b: Long): Long = {
      i match {
        case 0 => a
        case _ => r(i - 1, b, (a + b) % mod)
      }
    }

    r(n, 0, 1)
  }

}
