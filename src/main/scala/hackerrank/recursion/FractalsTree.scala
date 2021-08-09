package hackerrank.recursion

import scala.annotation.tailrec


object FractalsTree {

  def main(args: Array[String]): Unit = {
    (0 to 5).foreach { i =>
      println(s"case $i")
      drawTree(i)
      println("\n<><><>\n")
    }

  }

  def drawTree(n: Int): Unit=
    buildTree(n)
      .foreach(row => println(row.mkString("")))

  def buildTree(n: Int): Array[Array[Char]] = {

    def recursiveTree(n: Int, trunkHeight: Int, treeRoot: Int, base: Int, result: Array[Array[Char]]): Array[Array[Char]] = {
      if ( n > 0 ) {
        List
          .range(base, base - trunkHeight, -1)
          .foreach { i =>
            result(i)(treeRoot) = '1'
          }
        @tailrec
        def fillBranch(row: Int, leftColumn: Int, rightColumn: Int, i: Int): Unit = {
          if ( i != 0) {
            result(row)(leftColumn) = '1'
            result(row)(rightColumn) = '1'
            fillBranch(row - 1, leftColumn - 1, rightColumn + 1, i - 1)
          }
        }
        fillBranch(base - trunkHeight, treeRoot - 1, treeRoot + 1, trunkHeight)
        recursiveTree(n-1, trunkHeight/2, treeRoot - trunkHeight, base - (trunkHeight * 2), result)
        recursiveTree(n-1, trunkHeight/2, treeRoot + trunkHeight, base - (trunkHeight * 2), result)
      }
      result
    }
    recursiveTree(
      n = n,
      trunkHeight = 16,
      treeRoot = 49,
      base = 62,
      result = Array.fill(63, 100)('_')
    )
  }

}
