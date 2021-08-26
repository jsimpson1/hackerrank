package hackerrank.recursion.functionalstructures

trait UnionFind {

  def union(t1: Int, t2: Int): UnionFind

  def connected(t1: Int, t2: Int): Boolean

}
