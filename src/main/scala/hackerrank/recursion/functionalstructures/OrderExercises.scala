package hackerrank.recursion.functionalstructures

import java.sql.Time
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.util.Date
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.language.implicitConversions

object OrderExercises {

  Queue

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

  def nextTempSubArray(currentSum: Int, subArray: SubArray, alreadyDone: List[SubArray] ): SubArray = {
    val incrementedEndIndex = subArray.parentEndIndex + 1

    lazy val resetSum = -1

    alreadyDone
      .find(sa =>
        sa.parentStartIndex <= incrementedEndIndex && incrementedEndIndex <= sa.parentEndIndex
      ) match {
        case None =>
          if ( currentSum < 0) {
            subArray
              .copy(
                sum = resetSum,
                parentStartIndex = incrementedEndIndex,
                parentEndIndex = incrementedEndIndex,
              )
          } else {
            subArray
              .copy(
                parentEndIndex = incrementedEndIndex,
              )
          }
        case Some(ignored) =>
          val nextIndex = ignored.parentEndIndex + 1
          subArray
            .copy(
              sum =  resetSum,
              parentStartIndex = nextIndex,
              parentEndIndex = nextIndex,
            )
      }
  }

  @tailrec
  def firstViableStartIndex(index: Int, maxIndex: Int, alreadyDone: List[SubArray]): Option[Int] = {
    if ( index > maxIndex ) {
      None
    } else {
      alreadyDone
        .find( arr =>
          arr.parentStartIndex <= index &&  index <= arr.parentEndIndex
        ) match {
        case None =>
          Some(index)
        case Some(ignored) =>
          firstViableStartIndex(ignored.parentEndIndex + 1, maxIndex, alreadyDone)
      }
    }
  }

  val timeFomat = new SimpleDateFormat("hh:mm:ss")

  def calMaxSumSubArray(segmentedTree: SegmentedTree[Int], alreadyDone: List[SubArray]): Option[SubArray] = {

    println(s"calMaxSumSubArray -- start ${timeFomat.format(new Date())}")

    @tailrec
    def r(
      temp: SubArray,
      max: SubArray,
    ): SubArray = {
      if (temp.parentEndIndex > segmentedTree.maxIndex ) {
        max
      } else {
        val currentSum: Int =
          SegmentedTree
            .query(segmentedTree, temp.parentStartIndex, temp.parentEndIndex )

        if ( currentSum > max.sum ) {
          val nextMax = temp.copy(sum = currentSum)
          r(nextMax.incrementEndIndex, nextMax)
        } else {
          val nextTemp = nextTempSubArray(currentSum, temp, alreadyDone)
          r(nextTemp, max)
        }
      }
    }

    val initialTemp = firstViableStartIndex(0, segmentedTree.maxIndex, alreadyDone)

    initialTemp match {
      case None =>
        println(s"calMaxSumSubArray -- None end ${timeFomat.format(new Date())}, alreadyDone=${alreadyDone.map(v => v.parentEndIndex - v.parentStartIndex).sum}")
        None
      case Some(startIndex) =>
        if ( alreadyDone.flatMap(sa => (sa.parentStartIndex to sa.parentEndIndex)).size ==  segmentedTree.maxIndex + 1) {
          None
        } else {
          val initialSubArray = SubArray(0,startIndex,startIndex)
          val result = Some(r(initialSubArray, initialSubArray))
          println(s"calMaxSumSubArray -- Some end ${timeFomat.format(new Date())}, alreadyDone=${alreadyDone.map(v => v.parentEndIndex - v.parentStartIndex).sum}")
          result
        }
    }
  }

  def calcSubArrays(input: Input): Result = {

    def emptyValue: Int = 0

    lazy val segmentedTree: SegmentedTree[Int] =
      SegmentedTree
        .build[Int](
          input.arr,
          emptyValue,
        )

    @tailrec
    def r(result: Result): Result = {
      calMaxSumSubArray(segmentedTree, result.subArrays) match {
        case None =>
          result
        case Some(subArray) =>
          r(result.copy(subArrays = subArray :: result.subArrays))
      }
    }

    val result = r(Result(input.k, Nil))
    result.copy(subArrays = result.subArrays.filter(_.sum > 0))
  }

  object model {

    case class Input(n: Int, k: Int, arr: List[Int])

    case class Value(value: Int, index: Int)

    case class SubArray(
      sum: Int,
      parentStartIndex: Int,
      parentEndIndex: Int
    ) {

      def indexes: Range = (parentStartIndex to parentEndIndex)

      def incrementEndIndex: SubArray =
        this
          .copy(parentEndIndex = parentEndIndex + 1)

      def resetSum: SubArray =
        this
          .copy(sum = -1, parentStartIndex = parentEndIndex + 1, parentEndIndex = parentEndIndex + 1)

      def isEmpty: Boolean =
        parentStartIndex == parentEndIndex

    }

    case class Result(k: Int, subArrays: List[SubArray]) {

      def subArrayRanges: List[Range] = subArrays.map(_.indexes)

      lazy val sums: List[Int] =
        subArrays
          .map(_.sum)

      def sortedResult: List[Int] =
        subArrays
          .map{v => v.sum }
          .sorted(Ordering[Int].reverse)
          .take(k)

      def print(): Unit =
        println(sortedResult.mkString("\n"))

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
