package hackerrank.functionalprogramming.functionalstructures

import scala.annotation.tailrec
import scala.math.{Ordered, pow}

object SwapNodes {

  import model._
  import SwapNodeInputs._

  def main(args: Array[String]): Unit = {

    val si = parseInput(case2.split("\n"))

    println(si)

    val initialTree = buildTree(si.nodes)

    println(s"initialTree=${initialTree}")

    val swappedTree = swapNodes(si.swaps, initialTree)

    println(s"swappedTree=${swappedTree}")

  }

  def swapNodesFromLines(lines: Array[String]): Tree = {
    val inputs = parseInput(lines)
    inputs
      .swaps
      .foldLeft(buildTree(inputs.nodes)){ (tree, swap) =>
        tree.swap(swap.depth)
      }
  }

  def buildTreeFromLines(lines: Array[String]): Tree = {
    val inputs = parseInput(lines)
    buildTree(inputs.nodes)
  }

  def parseInput(lines: Array[String]): SwapInput = {

    // numOfNodes
    val N = lines.head.toInt

    val nodes: IndexedSeq[InputNode] =
      List.range(1, N + 1).map { i =>
        val nums: Array[String] = lines(i).split(" ")
        InputNode(nums(0).toInt, nums(1).toInt)
      }.toIndexedSeq

    // numOfSwaps
    val K = lines(N + 1).toInt

    val swapLevels: List[Swap] =
      List.range(1, K + 1).map { i =>
        Swap(lines(N + 1 + i).toInt)
      }
    SwapInput(nodes, swapLevels)
  }

  def swapNodes(swaps: List[Swap], tree: Tree): Tree = {
    swaps
      .foldLeft(tree) { (tree, swap) =>
        tree.swap(swap.depth)
      }
  }

  def buildTree(inputNodes: IndexedSeq[InputNode]): Tree = {

    def r(index: Int): Tree = index match {
      case model.emptyNodeValue =>
        EmptyNode
      case _ =>
        val previousNode = inputNodes(index - 1 )
        Node(index, r(previousNode.leftChildIndex), r(previousNode.rightChildIndex))
    }
    r(1)
  }

  object model {

    case class Swap(depth: Int)

    case class InputNode(leftChildIndex: Int, rightChildIndex: Int)

    case class SwapInput(nodes: IndexedSeq[InputNode], swaps: List[Swap])

    trait Tree {

      def value: Int

      def sortedNodes: List[Tree]

      def swap(i: Int, depth: Int = 1): Tree

    }

    val emptyNodeValue: Int = -1

    object EmptyNode extends Tree {

      override def value: Int = emptyNodeValue

      override def sortedNodes: List[Tree] = Nil

      override def swap(i: Int, depth: Int): Tree = this

      override def toString: String = "EmptyNode"
    }

    case class Node(
      value: Int,
      left: Tree,
      right: Tree,
    ) extends Tree {

      override def sortedNodes: List[Tree] =
        (left.sortedNodes :+ this) ++ right.sortedNodes

      override def swap(k: Int, depth: Int): Tree = {
        val (l, r) = if (depth % k == 0) (right, left) else (left, right)
        Node(value, l.swap(k, depth + 1), r.swap(k, depth + 1))
      }
    }
  }

}
