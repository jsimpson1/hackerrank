package hackerrank.functionalprogramming.recursion

import scala.annotation.tailrec
import scala.math.pow

object Playground {


  def main(args: Array[String]): Unit = {

    val inputs: List[Long] = List(0, 1, 5, 10, 100)

    inputs.foreach(v =>
      println(s"${v} = ${fibonacci(v)}")
    )

  }

  def fibonacci(n: Long): Long = {
    @tailrec
    def r(i: Long, a: Long, b: Long): Long = {
      i match {
        case 0 => a % (pow(10, 8) + 7).toLong
        case _ => r(i - 1, b, (a + b))
      }
    }

    r(n, 0, 1)
  }


}
