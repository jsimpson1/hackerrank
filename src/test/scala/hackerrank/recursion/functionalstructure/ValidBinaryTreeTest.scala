package hackerrank.recursion.functionalstructure

import hackerrank.recursion.functionalstructures.ValidBinarySearchTree
import org.scalatest.funsuite.AnyFunSuite

class ValidBinaryTreeTest extends AnyFunSuite {

  test("case empty") {
    val input = Nil
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(true)(actual)
  }

  test("case 0 ") {
    val input = List(1, 2, 3)
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(true)(actual)
  }

  test("case 1") {
    val input = List(2, 1, 3)
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(true)(actual)
  }

  test("case 3") {
    val input = List(3,2,1,5,4,6)
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(true)(actual)
  }

  test("case 4") {
    val input = List(1,3,4,2)
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(false)(actual)
  }

  test("case 5") {
    val input = List(3,4,5,1,2)
    val actual = ValidBinarySearchTree.isValidBst(input)
    assertResult(false)(actual)
  }

}
