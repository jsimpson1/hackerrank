package hackerrank.functionalprogramming.memoizationanddp

import scala.annotation.tailrec

object DifferentWays {

  def solve: Unit = {
    val numOfCases = readInt
    List
      .range(0, numOfCases)
      .foreach { _ =>
        val line = readLine
        val lineAsInts = line.split(" ").map(_.toInt)
        val poolSize = lineAsInts(0)
        val groupingSize = lineAsInts(1)
        val result = calcCombinationPermutationModule(poolSize, groupingSize)
        println(result)
      }
  }

  def solve(inputStr: String): List[BigInt] = {
    val casesLines: List[String] = inputStr.split("\n").tail.toList
    val cases: List[(Int, Int)] =
      casesLines
        .map(
          _.split(" ").map(_.toInt) match {
            case Array(poolSize, groupingSize) =>
              (poolSize, groupingSize)
          }
        )
    val combinations = cases.map(v => calcCombinationPermutationModule(v._1, v._2))
    combinations
  }

  def calcCombinationPermutationModule(poolSize: Int, groupingSize: Int): BigInt = {
    val answer: BigInt = calcCombinationPermutation(poolSize, groupingSize)
    val factorLong = (Math.pow(10, 8) + 7).toInt
    val factor: BigInt = BigInt(factorLong)
    val result = (answer % factor)
    result
  }

  // nCr = n! / r! (n - r)!
  private def calcCombinationPermutation(poolSize: Int, groupingSize: Int): BigInt = {
    if ( groupingSize == 0 ) {
      1
    } else if ( poolSize == groupingSize ){
      1
    } else {
      factorial(poolSize) / (factorial(groupingSize) * factorial(poolSize - groupingSize))
    }
  }

  private def factorial(i: Int): BigInt = {
    @tailrec
    def r(i: Long, result: BigInt): BigInt = {
      val nextNumDown: Long  = i - 1
      if ( nextNumDown < 1) {
        result
      } else {
        r(nextNumDown, result * BigInt(nextNumDown))
      }
    }

    if ( i > 1 ) {
      r(i, i)
    } else {
      1
    }
  }

}
