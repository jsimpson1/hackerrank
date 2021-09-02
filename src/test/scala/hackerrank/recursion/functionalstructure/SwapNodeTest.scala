package hackerrank.recursion.functionalstructure

import hackerrank.recursion.functionalstructures.{SwapNodeInputs, SwapNodes}
import hackerrank.recursion.functionalstructures.SwapNodes.model._
import org.scalatest.funsuite.AnyFunSuite

class SwapNodeTest extends AnyFunSuite {

  test("case 0 - buildTree") {
    val input = SwapNodeInputs.case0

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(1,Some(Node(2,None,None,1)), Some(Node(3,None,None,1)),0),
      Node(2,None,None,1),
      Node(3,None,None,1)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  ignore("case 0 - swapNodes") {
    val input = SwapNodeInputs.case0

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(1,Some(Node(3,None,None,1)),Some(Node(2,None,None,1)),0),
      Node(2,None,None,1),
      Node(3,None,None,1)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 1 - buildTree") {
    val input = SwapNodeInputs.case1

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(1,Some(Node(2,None,None,1)),Some(Node(3,None,None,1)),0),
      Node(2,None,Some(Node(4,None,None,2)),1),
      Node(3,None,Some(Node(5,None,None,2)),1),
      Node(4,None,None,2),
      Node(5,None,None,2)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  ignore("case 1 - swapNodes") {
    val input = SwapNodeInputs.case1

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(1,Some(Node(2,None,None,1)),Some(Node(3,None,None,1)),0),
      Node(2,Some(Node(4,None,None,2)), None,1),
      Node(3,Some(Node(5,None,None,2)), None,1),
      Node(4,None,None,2),
      Node(5,None,None,2))

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 2 - buildTree") {
    val input = SwapNodeInputs.case2

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(1,Some(Node(2,None,None,1)),Some(Node(3,None,None,1)),0),
      Node(2,Some(Node(4,None,None,2)), None,1),
      Node(3,Some(Node(5,None,None,2)), None,1),
//      Node(4,Some(Node(6,None,Some(Node(9,None,None,4)),3)), None,2),
      Node(4,Some(Node(6,None,None,3)), None,2),
//      Node(5,Some(Node(7,None,None,3)), Some(Node(8,Some(Node(10,None,None,4)),Some(Node(11,None,None,4)),3)),2),
      Node(5,Some(Node(7,None,None,3)), Some(Node(8,None,None,3)),2),
      Node(6,None, Some(Node(9,None,None,4)),3),
      Node(7,None, None,3),
      Node(8,Some(Node(10,None,None,4)), Some(Node(11,None,None,4)),3),
      Node(9,None, None,4),
      Node(10,None, None,4),
      Node(11,None, None,4),
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  ignore("case 2 - swapNodes") {
    val input = SwapNodeInputs.case2

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected = List(
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

}
