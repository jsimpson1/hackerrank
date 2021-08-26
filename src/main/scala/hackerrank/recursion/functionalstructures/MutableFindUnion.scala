package hackerrank.recursion.functionalstructures

import hackerrank.recursion.functionalstructures

import scala.annotation.tailrec

class MutableFindUnion(val size: Int) {

  private case class Node(var parent: Option[Int], var treeSize: Int)

  private val nodes = Array.fill[Node](size)(Node(None, 1))

  def union(t1: Int, t2: Int): MutableFindUnion = {
    if (t1 == t2) return this

    val root1 = root(t1)
    val root2 = root(t2)
    if (root1 == root2) return this

    val node1 = nodes(root1)
    val node2 = nodes(root2)

    if (node1.treeSize < node2.treeSize) {
      node1.parent = Some(t2)
      node2.treeSize += node1.treeSize
    } else {
      node2.parent = Some(t1)
      node1.treeSize += node2.treeSize
    }
    this
  }

  def find(t1: Int, t2: Int): Boolean =
    t1 == t2 || root(t1) == root(t2)

  @tailrec
  private def root(t: Int): Int =
    nodes(t)
      .parent match {
      case None => t
      case Some(p) => root(p)
    }

  def findParent(t: Int): Int = root(t)

}
