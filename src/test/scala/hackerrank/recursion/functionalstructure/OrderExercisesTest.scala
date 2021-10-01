package hackerrank.recursion.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.OrderExercises._
import hackerrank.functionalprogramming.functionalstructures.OrderExercises.model._
import hackerrank.functionalprogramming.functionalstructures.model.{Leaf, Node, SegmentedTree}
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.language.implicitConversions

class OrderExercisesTest extends AnyFunSuite {

  val printResults = true

  def printResults[A](expected: A, actual: A): Unit = {
    if ( printResults )
      println(
        s"""expected = $expected
           |actual   = $actual""".stripMargin
      )
  }

  def emptyValue: Int = 0

  test("SegmentedTree build") {

    val actualTree: SegmentedTree[Int] = SegmentedTree.build[Int](1,2,0)

    val expectedTree = Node(1,2,0,Leaf(1,0), Leaf(2,0))

    assertResult(0)(SegmentedTree.query(actualTree, 0, 0))
    assertResult(0)(SegmentedTree.query(actualTree, 1, 1))
    assertResult(0)(SegmentedTree.query(actualTree, 1, 2))

    assertResult(expectedTree)(actualTree)
  }

  test("SegmentedTree update leaf") {

    val tree: SegmentedTree[Int] = Leaf(0,0)

    val actual = SegmentedTree.update[Int](tree, 0, 1)

    val expected = Leaf(0,1)

    printResults(expected, actual)

    assertResult(expected)(actual)

  }

  test("SegmentedTree update trivial left node") {

    val tree: SegmentedTree[Int] = Node(0,1,0, Leaf(0,0), Leaf(1,0))

    val actual = SegmentedTree.update[Int](tree, 0, 1)

    val expected = Node(0,1,1,Leaf(0,1), Leaf(1,0))

    printResults(expected, actual)

    assertResult(expected)(actual)

  }

  test("SegmentedTree update trivial right node") {

    val tree: SegmentedTree[Int] = Node(0,1,0, Leaf(0,0), Leaf(1,0))

    val actual = SegmentedTree.update[Int](tree, 1, 1)

    val expected = Node(0,1,1,Leaf(0,0), Leaf(1,1))

    printResults(expected, actual)

    assertResult(expected)(actual)

  }

  test("SegmentedTree update multiple nodes") {

    val tree: SegmentedTree[Int] = Node(0,3,0, Node(0,1,0, Leaf(0,0), Leaf(1,0)), Leaf(2,0))

    val actual0: SegmentedTree[Int] = SegmentedTree.update[Int](tree, 1, 1)

    val expected0: SegmentedTree[Int] = Node(0,3,1, Node(0,1,1, Leaf(0,0), Leaf(1,1)), Leaf(2,0))

    assert(actual0 == expected0)

    val actual1: SegmentedTree[Int] = SegmentedTree.update[Int](actual0, 2, 1)

    val expected1: SegmentedTree[Int] = Node(0,3,2, Node(0,1,1, Leaf(0,0), Leaf(1,1)), Leaf(2,1))

    assert(actual1 == expected1)

    val actual2: SegmentedTree[Int] = SegmentedTree.update[Int](actual1, 0, 2)

    val expected2: SegmentedTree[Int] = Node(0,3,4, Node(0,1,3, Leaf(0,2), Leaf(1,1)), Leaf(2,1))

    assert(actual2 == expected2)

  }

  test("SegmentedTree build with values") {

    val values = List(1, 3, -2, 8, -7)

    val initialRoot = SegmentedTree.build(0, 4, 0)

    val expectedInitialRoot = Node(
      0,4,0,
      leftChild = Node(
        0,2,0,
        leftChild = Node(
          0,1,0,
          Leaf(0,0),
          Leaf(1,0)
        ),
        rightChild = Leaf(2,0)
      ),
      rightChild = Node(
        3,4,0,
        leftChild = Leaf(3,0),
        rightChild = Leaf(4,0)
      )
    )

    assert(initialRoot == expectedInitialRoot)

    val actualTree: SegmentedTree[Int] = SegmentedTree.build[Int](values, emptyValue)

    val expectedTree: SegmentedTree[Int] = Node(
        0,4,3,
        leftChild = Node(
          0,2,2,
          leftChild = Node(
            0,1,4,
            Leaf(0,1),
            Leaf(1,3)
          ),
          rightChild = Leaf(2,-2)
        ),
        rightChild = Node(
          3,4,1,
          leftChild = Leaf(3,8),
          rightChild = Leaf(4,-7)
        )
      )

    assert( actualTree == expectedTree)
  }

  test("SegmentedTree query") {

    val arr = List(-2, -3, 4)

    val segmentedTree: SegmentedTree[Int] = SegmentedTree.build[Int](arr, emptyValue)

    assert( SegmentedTree.query(segmentedTree, 0, 0 ) == -2)
    assert( SegmentedTree.query(segmentedTree, 1, 1 ) == -3)
    assert( SegmentedTree.query(segmentedTree, 2, 2 ) == 4)

    assert( SegmentedTree.query(segmentedTree, 0, 2 ) == -1)

  }

  test("SegmentedTree query case 0") {

    val arr = List(-2, -3, 4, -1, -2, 1, 5, -3)

    val segmentedTree: SegmentedTree[Int] = SegmentedTree.build[Int](arr, emptyValue)

    assert( SegmentedTree.query(segmentedTree, 0, 0 ) == -2)
    assert( SegmentedTree.query(segmentedTree, 1, 1 ) == -3)
    assert( SegmentedTree.query(segmentedTree, 2, 2 ) == 4)
    assert( SegmentedTree.query(segmentedTree, 3, 3 ) == -1)
    assert( SegmentedTree.query(segmentedTree, 4, 4 ) == -2)
    assert( SegmentedTree.query(segmentedTree, 5, 5 ) == 1)
    assert( SegmentedTree.query(segmentedTree, 6, 6 ) == 5)
    assert( SegmentedTree.query(segmentedTree, 7, 7 ) == -3)

    assert( SegmentedTree.query(segmentedTree, 0, 1 ) == -5)
    assert( SegmentedTree.query(segmentedTree, 1, 2 ) == 1)
    assert( SegmentedTree.query(segmentedTree, 2, 3 ) == 3)
    assert( SegmentedTree.query(segmentedTree, 3, 4 ) == -3)
    assert( SegmentedTree.query(segmentedTree, 4, 5 ) == -1)
    assert( SegmentedTree.query(segmentedTree, 5, 6 ) == 6)
    assert( SegmentedTree.query(segmentedTree, 6, 7 ) == 2)

    assert( SegmentedTree.query(segmentedTree, 0, 2 ) == -1)
    assert( SegmentedTree.query(segmentedTree, 1, 3 ) == 0)

    assert( SegmentedTree.query(segmentedTree, 1, 6 ) == 4)


    assert( SegmentedTree.query(segmentedTree, 0, 7 ) == -1)

  }

  test("calMaxSum case 0") {

    val arr = List(-2, -3, 4, -1, -2, 1, 5, -3)

    val segmentedTree: SegmentedTree[Int] = SegmentedTree.build[Int](arr, emptyValue)

    val actual = calMaxSumSubArray(segmentedTree, 0, arr.size - 1)

    val expected = SubArray(7, 2, 6)

    assertResult(expected)(actual)

  }

  test("case 0") {
    val inputStr = """5 3
                 |2 4 -10 2 -2""".stripMargin

    val actual: List[Int] = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(6,2)

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 1") {
    val inputStr = """4 2
                     |-2 5 -1 -8""".stripMargin

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(5)

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 3") {
    val inputStr = """100 16
                     |9404 8036 -9334 -9146 8085 3024 988 5875 2264 -4643 -8916 -8072 1954 3424 5364 -2633 -8910 -7310 9443 -5096 4982 -7834 5164 -8360 185 265 277 -4154 -6615 6233 5988 -9008 5849 -948 6458 -9633 7955 432 1308 6533 -4667 9545 9446 1002 4452 -2285 -2413 -8734 4224 5492 -6250 -38 -3089 -6761 2326 -2209 -7962 -929 5710 -391 -6415 5399 4758 933 -3318 -8572 566 8181 -1512 -2937 -5897 5525 -7054 912 -8863 -4893 2963 -8827 -8376 5579 -8906 2265 5349 9388 4664 5708 2630 -4177 7665 6774 -5152 -5504 6138 2018 2464 3936 -5985 9804 -9520 1245""".stripMargin

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected = List(
      47985,
      40945,
      20236,
      17440,
      11090,
      10742,
      9716,
      9443,
      8747,
      5710,
      5579,
      5525,
      5164,
      4982,
      2963,
      2326,
    )

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 6") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase6.txt")
      s.mkString
    }

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase6Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    }

    printResults(expected, actual)

    assertResult(expected)(actual)
  }

  test("case 7") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase7.txt")
      s.mkString
    }

    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase7Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    }

  val actual = calcSubArrays(parseInput(inputStr)).sortedResult

//    println(s"actual diff expected ${actual.diff(expected)}")
//
//    println(s"expected diff actual ${expected.diff(actual)}")

    assertResult(expected)(actual)
  }

  test("case 16") {
    val inputStr: String = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase16.txt")
      s.mkString
    }

    val actual = calcSubArrays(parseInput(inputStr)).sortedResult
    val expected: List[Int] = {
      val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/orderExercisesCase16Result.txt")
      s
        .mkString
        .split("\n")
        .map(_.toInt)
        .toList
    }

//    printResults(expected, actual)

//    println(s"actual diff expected ${actual.diff(expected)}")
//
//    println(s"expected diff actual ${expected.diff(actual)}")

    assertResult(expected)(actual)
  }

}
