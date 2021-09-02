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

    def resolveValue(i: Int): Option[Int] =
      if (i == -1) None
      else Some(i)


    def buildNode(parent: Int, children: InputNode, depth: Int): Option[Node] = {
      resolveValue(parent)
        .map { parentValue =>
          val leftNode = resolveValue(children.leftChildIndex).map(i => Node(i, None, None, depth + 1))
          val rightNode = resolveValue(children.rightChildIndex).map(i => Node(i, None, None, depth + 1))
          Node(parentValue, leftNode, rightNode, depth)
        }
    }

    def buildLevel(levelsParents: List[InputNode], levelsChildren: List[InputNode], depth: Int): List[Node] = {
      if ( levelsParents.size != levelsChildren.size) {
        throw new RuntimeException("pattern being used to build tree is wrong.")
      } else {
        val children =
        levelsParents.zip(levelsChildren)
          .flatMap{ case (parent, children) =>
            val leftChild = buildNode(parent.leftChildIndex,children, depth)
            val rightChild = buildNode(parent.rightChildIndex,children, depth)
            List(leftChild, rightChild).flatten
          }
          children
      }
    }

    def buildLevelOne(parent: InputNode, leftChild: InputNode, rightChild: InputNode): List[Node] = {
      val depth = 1
      val leftParent = buildNode(parent.leftChildIndex, leftChild, depth)
      val rightParent = buildNode(parent.rightChildIndex, rightChild, depth)
      List(leftParent, rightParent).flatten
    }

    @tailrec
    def r(nodes: List[InputNode], level: Int, result: List[Node]): List[Node] = {
      level match {
        case 1 =>
          nodes match {
            case Nil =>
              List(Node(1, None, None, 1))
            case h :: Nil =>
              val updatedRoot = buildNode(1, h, 0).get
              r(nodes.drop(1), level + 1, List(updatedRoot))
            case h :: tail =>
              val updatedRoot = buildNode(1, h, 0).get
              val levelOneNodes = buildLevelOne(h, tail.head, tail.tail.head)
              r(nodes.drop(1), level + 1, updatedRoot :: levelOneNodes)
          }
        case depth =>
          val numOfNodesForDepth = level * 2
          val nodesForDepth: List[InputNode] = nodes.take(numOfNodesForDepth)

          if ( nodesForDepth.forall(in => in.leftChildIndex == -1 && in.rightChildIndex == -1) ) {
            val nodesWithNoChildren: List[Node] =
              result
                .takeRight(nodesForDepth.size)
                .flatMap{ node =>
                  List(node.left, node.right)
                    .flatten
                }
              result ++ nodesWithNoChildren
          } else {
            val parentsAndChildren: (List[InputNode], List[InputNode]) = nodesForDepth.splitAt(level)
            val depthsParents: List[InputNode] = parentsAndChildren._1
            val depthsChildren: List[InputNode] = parentsAndChildren._2
            val nodesForLevel = buildLevel(depthsParents, depthsChildren, depth)
            r(nodes.drop(numOfNodesForDepth), level + 1, result ++ nodesForLevel )
          }
      }
    }

    Tree(
      nodes = r(inputNodes, 1, Nil)
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
