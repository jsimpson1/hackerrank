package hackerrank.recursion.functionalstructures

import scala.annotation.tailrec

object ImmutableFindUnion {

  def create(size: Int): ImmutableFindUnion = {
    val nodes = Vector.fill(size)(Node(None, 1))
    new ImmutableFindUnion(nodes)
  }

}

case class Node(parent: Option[Int], treeSize: Int)

class ImmutableFindUnion(nodes: Vector[Node]) extends UnionFind {

  def union(t1: Int, t2: Int): ImmutableFindUnion = {
    if (t1 == t2) return this

    val root1 = root(t1)
    val root2 = root(t2)
    if (root1 == root2) return this

    val node1 = nodes(root1)
    val node2 = nodes(root2)
    val newTreeSize = node1.treeSize + node2.treeSize

    val (newNode1, newNode2) =
      if (node1.treeSize < node2.treeSize) {
        val newNode1 = Node(Some(t2), newTreeSize)
        val newNode2 = Node(node2.parent, newTreeSize)

        (newNode1, newNode2)
      } else {
        val newNode2 = Node(Some(t1), newTreeSize)
        val newNode1 = Node(node1.parent, newTreeSize)

        (newNode1, newNode2)
      }
    val newNodes = nodes.updated(root1, newNode1).updated(root2, newNode2)
    new ImmutableFindUnion(newNodes)
  }

  def connected(t1: Int, t2: Int): Boolean =
    t1 == t2 || root(t1) == root(t2)

  @tailrec
  private def root(t: Int): Int =
    nodes(t)
      .parent match {
      case None => t
      case Some(p) => root(p)
    }

}
