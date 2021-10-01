package hackerrank.functionalprogramming.demo

import scala.annotation.tailrec

object BubbleSort extends App {

  def bubbleSort(arr: List[Int]): List[Int] = {
    @tailrec
    def r(source: List[Int], iteration: List[Int], result: List[Int]): List[Int] = {
      source match {
        case h1 :: h2 :: tail =>
          if ( h1 > h2) {
            println("h1 > h2")
            r(h1 :: tail, h2 :: iteration, result)
          } else {
            println("h1 <= h2")
            r(h2 :: tail, h1 :: iteration, result)
          }
        case h1 :: Nil =>
          println("h1 :: Nil")
          r(iteration, Nil, h1 :: result)
        case Nil =>
          println("done")
          result
      }
    }
    r(arr, Nil, Nil)

  }

}
