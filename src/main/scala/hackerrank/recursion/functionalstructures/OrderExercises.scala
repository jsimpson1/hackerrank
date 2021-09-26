package hackerrank.recursion.functionalstructures

import scala.annotation.tailrec
import scala.language.implicitConversions

object OrderExercises {

  import model._

  def main(args: Array[String]): Unit = {
    val inputStr = """5 3
                  |2 4 -10 2 -2""".stripMargin

    val input = parseInput(inputStr)

    println(input)

    val result = calcSubArrays(input)

    result.print()

  }

  def parseInput(str: String): Input = {
    val lines = str.split("\n")
    val nk = lines.head.split(" ")
    val arr = lines(1).split(" ").map(_.toInt).toList
    Input(nk(0).toInt, nk(1).toInt, arr)
  }

  implicit def sum(a: Int, b: Int): Int = a + b

  def calMaxSumSubArray(segmentedTree: SegmentedTree[Int], startIndex: Int, endIndex: Int): SubArray = {

    @tailrec
    def r(
      temp: SubArray,
      max: SubArray,
    ): SubArray = {
      if (temp.parentEndIndex > endIndex ) {
        max
      } else {
        val currentSum: Int =
          SegmentedTree
            .query(segmentedTree, temp.parentStartIndex, temp.parentEndIndex )

        if ( currentSum > max.sum ) {
          val nextMax = temp.copy(sum = currentSum)
          r(nextMax.incrementEndIndex, nextMax)
        } else if ( currentSum < 0) {
          r(temp.resetSum, max)
        } else {
          r(temp.incrementEndIndex, max)
        }
      }
    }

    val initialSubArray = SubArray(0, startIndex, startIndex)

    r(initialSubArray, initialSubArray)
  }

  def calcSubArrays(input: Input): Result = {

    def emptyValue: Int = 0

    lazy val segmentedTree: SegmentedTree[Int] =
      SegmentedTree
        .build[Int](
          input.arr,
          emptyValue,
        )

    def r(startIndex: Int, endIndex: Int, result: Set[SubArray]): Set[SubArray] = {
      if ( startIndex >= endIndex ) {
        result
      } else {
        val subArray = calMaxSumSubArray(segmentedTree, startIndex, endIndex)

        lazy val nextResult: Set[SubArray] =
          if ( subArray.sum > 0 )
            result + subArray
          else
            result

        r(startIndex, subArray.parentStartIndex - 1, nextResult) ++ r(subArray.parentEndIndex + 1, endIndex, nextResult)
      }
    }

    Result(
      input.k,
      r(0, segmentedTree.maxIndex, Set()).toList
    )
  }

  object model {

    case class Input(n: Int, k: Int, arr: List[Int])

    case class SubArray(
      sum: Int,
      parentStartIndex: Int,
      parentEndIndex: Int
    ) {

      def incrementEndIndex: SubArray =
        this
          .copy(parentEndIndex = parentEndIndex + 1)

      def resetSum: SubArray =
        this
          .copy(sum = -1, parentStartIndex = parentEndIndex + 1, parentEndIndex = parentEndIndex + 1)

    }

    case class Result(k: Int, subArrays: List[SubArray]) {

      def sortedResult: List[Int] =
        subArrays
          .map{v => v.sum }
          .sorted(Ordering[Int].reverse)
          .take(k)

      def print(): Unit =
        println(
          sortedResult
            .mkString("\n")
        )

    }

    abstract class SegmentedTree[T] {
      def value: T
      def maxIndex: Int
    }

    object SegmentedTree {

      def middle(
        leftIndex: Int,
        rightIndex: Int,
      ): Int = (leftIndex + rightIndex) / 2

      def updateValues[T](
        initialTree: SegmentedTree[T],
        values: List[(Int, T)],
      )(
        implicit
        partialFn: (T, T) => T,
      ): SegmentedTree[T] = {

        @tailrec
        def r(
          tree: SegmentedTree[T],
          values: List[(Int, T)],
        ): SegmentedTree[T] = {
          values match {
            case Nil =>
              tree
            case h :: tail =>
              r(update(tree, h._1, h._2), tail)
          }
        }

        r(initialTree, values)
      }


      def update[T](
        tree: SegmentedTree[T],
        index: Int,
        value: T,
      )(
        implicit
          partialFn: (T, T) => T,
      ): SegmentedTree[T] =
        tree match {
          case Leaf(i, _) =>
              Leaf(i, value)
          case Node(l, r, _, lc, rc) =>
            if ( index <= middle(l,r) ) {
              val updatedLeftChild = update(lc, index, value)
              val updatedValue = partialFn(updatedLeftChild.value, rc.value)
              Node(l, r, updatedValue, updatedLeftChild, rc)
            } else {
              val updatedRightChild = update(rc, index, value)
              val updatedValue = partialFn(lc.value, updatedRightChild.value)
              Node(l, r, updatedValue, lc, updatedRightChild)
            }
        }

      def build[T](
        values: List[T],
        emptyValue: T,
      )(
        implicit
          partialFn: (T, T) => T,
      ): SegmentedTree[T] = {

        @tailrec
        def r(
          root: SegmentedTree[T],
          values: List[(T, Int)]
        ): SegmentedTree[T] = {
          values match {
            case Nil =>
              root
            case h :: tail =>
              val nextRoot = SegmentedTree.update(
                tree = root,
                index = h._2,
                value = h._1
              )
              r(
                nextRoot,
                tail
              )
          }
        }
        r(
          SegmentedTree
            .build(
              0,
              values.size - 1,
              emptyValue
            ),
          values.zipWithIndex
        )
      }

      def build[T](
        leftIndex: Int,
        rightIndex: Int,
        value: T,
      ): SegmentedTree[T] = {
        if ( leftIndex != rightIndex ) {
          Node(
            leftIndex,
            rightIndex,
            value,
            leftChild = build(
              leftIndex,
              middle(leftIndex, rightIndex),
              value
            ),
            rightChild = build(
              middle(leftIndex, rightIndex) + 1,
              rightIndex,
              value
            ),
          )
        } else {
          Leaf(leftIndex, value)
        }
      }

      def query[T](
        root: SegmentedTree[T],
        l: Int,
        r: Int,
      )(
        implicit
          partialFn: (T, T) => T,
      ): T =
        root match {
          case Leaf(_, value) =>
            value
          case Node(leftIndex, rightIndex, value, leftChild, rightChild) =>
            val isNode = leftIndex == l && rightIndex == r
            val isInLeft = r <= middle(leftIndex, rightIndex)
            val isInRight = l > middle(leftIndex, rightIndex)
            if ( isNode )
              value
            else if ( isInLeft )
              query(leftChild, l, r)
            else if ( isInRight )
              query(rightChild, l, r)
            else
              partialFn(
                query(
                  leftChild,
                  l,
                  middle(leftIndex, rightIndex),
                ),
                query(
                  rightChild,
                  middle(leftIndex, rightIndex) + 1,
                  r,
                )
              )
        }

    }

    case class Node[T](
      leftIndex: Int,
      rightIndex: Int,
      value: T,
      leftChild: SegmentedTree[T],
      rightChild: SegmentedTree[T],
    ) extends SegmentedTree[T] {
      override def maxIndex: Int = rightIndex
    }

    case class Leaf[T](
      index: Int,
      value: T
    ) extends SegmentedTree[T] {
      override def maxIndex: Int = index
    }

  }

}
