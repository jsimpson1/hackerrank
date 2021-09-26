package hackerrank.recursion.functionalstructures.model

import scala.annotation.tailrec

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
