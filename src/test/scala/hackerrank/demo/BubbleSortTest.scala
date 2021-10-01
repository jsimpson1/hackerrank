package hackerrank.demo

import hackerrank.functionalprogramming.demo.BubbleSort
import org.scalatest.funsuite.AnyFunSuite

class BubbleSortTest extends AnyFunSuite {

  test("no sort") {
    val input = List(1,2,3,4,5)
    val actual = BubbleSort.bubbleSort(input)
    println("---------------------")
    assertResult((input, 0))(actual)
  }

  test("sort ever step") {
    val input = List(5,4,3,2,1)
    val actual = BubbleSort.bubbleSort(input)
    println("---------------------")
    assertResult((List(1,2,3,4,5), 4))(actual)
  }

  test("a few sorts") {
    val input = List(5,4,2,3,1)
    val actual: List[Int] = BubbleSort.bubbleSort(input)
    println("---------------------")
    assertResult(List(1,2,3,4,5))(actual)
  }



}
