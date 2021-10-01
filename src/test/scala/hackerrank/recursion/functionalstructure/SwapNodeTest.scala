package hackerrank.recursion.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.{SwapNodeInputs, SwapNodes}
import hackerrank.functionalprogramming.functionalstructures.SwapNodes.model._
import org.scalatest.funsuite.AnyFunSuite

class SwapNodeTest extends AnyFunSuite {

  test("case 0 - buildTree") {
    val input = SwapNodeInputs.case0

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(2,EmptyNode,EmptyNode),
      Node(1,Node(2,EmptyNode,EmptyNode), Node(3,EmptyNode,EmptyNode)),
      Node(3,EmptyNode,EmptyNode)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 0 - swapNodes") {
    val input = SwapNodeInputs.case0

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(2,EmptyNode,EmptyNode),
      Node(1,Node(2,EmptyNode,EmptyNode), Node(3,EmptyNode,EmptyNode)),
      Node(3,EmptyNode,EmptyNode)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 1 - buildTree") {
    val input = SwapNodeInputs.case1

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(2,EmptyNode,Node(4,EmptyNode,EmptyNode)),
      Node(4,EmptyNode,EmptyNode),
      Node(1,Node(2,EmptyNode,Node(4,EmptyNode,EmptyNode)),Node(3,EmptyNode,Node(5,EmptyNode,EmptyNode))),
      Node(3,EmptyNode,Node(5,EmptyNode,EmptyNode)),
      Node(5,EmptyNode,EmptyNode)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 1 - swapNodes") {
    val input = SwapNodeInputs.case1

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected =
      List(
        Node(4,EmptyNode,EmptyNode),
        Node(2,Node(4,EmptyNode,EmptyNode),EmptyNode),
        Node(1,Node(2,Node(4,EmptyNode,EmptyNode),EmptyNode),Node(3,Node(5,EmptyNode,EmptyNode),EmptyNode)),
        Node(5,EmptyNode,EmptyNode),
        Node(3,Node(5,EmptyNode,EmptyNode),EmptyNode)
      )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 2 - buildTree") {
    val input = SwapNodeInputs.case2

    val actual = SwapNodes.buildTreeFromLines(input.split("\n")).sortedNodes

    val expected = List(
      Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),
      Node(9,EmptyNode,EmptyNode),
      Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode),
      Node(2,Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode),EmptyNode),
      Node(1,Node(2,Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode),EmptyNode),Node(3,Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode))),EmptyNode)),
      Node(7,EmptyNode,EmptyNode),
      Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode))),
      Node(10,EmptyNode,EmptyNode),
      Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode)),
      Node(11,EmptyNode,EmptyNode),
      Node(3,Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode))),EmptyNode)
    )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("case 2 - swapNodes") {
    val input = SwapNodeInputs.case2

    val actual = SwapNodes.swapNodesFromLines(input.split("\n")).sortedNodes

    val expected =
      List(
        Node(2,EmptyNode,Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode)),
        Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),
        Node(9,EmptyNode,EmptyNode),
        Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode),
        Node(1,Node(2,EmptyNode,Node(4,Node(6,EmptyNode,Node(9,EmptyNode,EmptyNode)),EmptyNode)),Node(3,EmptyNode,Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode))))),
        Node(3,EmptyNode,Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode)))),
        Node(7,EmptyNode,EmptyNode),
        Node(5,Node(7,EmptyNode,EmptyNode),Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode))),
        Node(10,EmptyNode,EmptyNode),
        Node(8,Node(10,EmptyNode,EmptyNode),Node(11,EmptyNode,EmptyNode)),
        Node(11,EmptyNode,EmptyNode)
      )

    println(s"actual:   ${actual}")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

}
