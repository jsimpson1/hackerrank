package hackerrank.recursion.functionalstructures

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
    swapNodes(inputs.swaps, buildTree(inputs.nodes))
  }

  def buildTreeFromLines(lines: Array[String]): Tree = {
    val inputs = parseInput(lines)
    buildTree(inputs.nodes)
  }

  def parseInput(lines: Array[String]): SwapInput = {

    // numOfNodes
    val N = lines.head.toInt

    val nodes: List[InputNode] =
      List.range(1, N + 1).map { i =>
        val nums: Array[String] = lines(i).split(" ")
        InputNode(nums(0).toInt, nums(1).toInt)
      }

    // numOfSwaps
    val K = lines(N + 1).toInt

    val swapLevels: List[Swap] =
      List.range(1, K + 1).map { i =>
        Swap(lines(N + 1 + i).toInt)
      }
    SwapInput(nodes, swapLevels)
  }

  def buildTree(inputNodes: List[InputNode]): Tree = {

    def resolveNode(i: Int, level: Int): Option[Node] =
      if (i == -1)
        None
      else
        Some(Node(i, None, None, level))

    @tailrec
    def r(nodes: List[InputNode], level: Int, result: List[Node]): List[Node] = {
      nodes match {
        case Nil =>
          result
        case values =>
          val numOfNodes = pow(2, level - 1).toInt
          result
            .partition(_.depth == level - 1) match {
            case (parentNodes, otherNodes) =>
              val nextNodes: List[Node] =
                values
                  .take(numOfNodes)
                  .zipWithIndex
                  .flatMap { case (in, index) =>
                    val leftNode: Option[Node] = resolveNode(in.leftChildIndex, level)
                    val rightNode: Option[Node] = resolveNode(in.rightChildIndex, level)
                    val nextParent: Option[Node] =
                      parentNodes
                        .lift(index) match {
                          case None =>
                            None
                          case Some(node) =>
                            Some(node.copy(left = leftNode, right = rightNode))
                        }
                    List(nextParent, leftNode, rightNode)
                  }.flatten
              r(nodes.drop(numOfNodes), level + 1, otherNodes ++ nextNodes)
          }


      }


    }

    Tree(
      nodes = r(inputNodes, 1, List(Node(1, None, None, 0)))
    )
  }

  @tailrec
  def swapNodes(swaps: List[Swap], tree: Tree): Tree = {
    swaps match {
      case Nil =>
        tree
      case h :: tail =>
        val nextTree: Tree =
          tree
            .nodes
            .partition(_.depth == h.depth - 1) match {
            case (parents, others) =>
              val nextParents =
                parents
                  .map { node =>
                    Node(node.value, node.right, node.left, node.depth)
                  }
              Tree(others ++ nextParents)
          }
        swapNodes(tail, nextTree)
    }
  }

  object model {

    case class Swap(depth: Int)

    case class InputNode(leftChildIndex: Int, rightChildIndex: Int)

    case class SwapInput(nodes: List[InputNode], swaps: List[Swap])

    case class Tree(nodes: List[Node]) {

      def sortedNodes: List[Node] = nodes.sorted

      override def toString: String = sortedNodes.toString

    }

    case class Node(
      value: Int,
      left: Option[Node],
      right: Option[Node],
      depth: Int
    ) extends Ordered[Node] {

      override def compare(that: Node): Int =
        depth.compare(that.depth)

    }
  }


}
