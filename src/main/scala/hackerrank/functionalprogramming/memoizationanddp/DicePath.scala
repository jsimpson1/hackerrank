package hackerrank.functionalprogramming.memoizationanddp

import scala.collection.mutable

object DicePath {

  import model._

  val initialDicePath: DicePath = model.DicePath(Dice.initialDice, 1)

  def solve(): Unit = {
    val sb = new StringBuilder

    val numOfCases = readInt

    sb.append(s"$numOfCases\n")

    List
      .range(0, numOfCases)
      .foreach(_ =>
        sb.append(s"$readLine\n")
      )

    val inputStr = sb.toString
    solveCase(inputStr)
  }

  def solveCase(inputStr: String): Unit = {
    val mnCases: List[MnKey] =
      inputStr
        .split("\n")
        .tail
        .map{ line =>
          line
            .split(" ")
            .map(_.toInt) match {
              case Array(m, n) =>
                MnKey(m, n)
            }
        }.toList
    val cache: mutable.HashMap[MnKey, Set[DicePath]] = mutable.HashMap(
      (MnKey(1,1), Set(initialDicePath))
    )
    val result = mnCases.map{ mnCase =>
      val result = sumOfMaximalPath(mnCase, cache)
      result
    }
    result.foreach(println)
  }

  private def rotateDicePath(
    key: MnKey,
    cache: mutable.HashMap[MnKey, Set[DicePath]]
  ) : Set[DicePath] = {

    lazy val rotateRight = rotateDicePath(key.nextRight, cache).map(_.rotateRight)

    lazy val rotateDown = rotateDicePath(key.nextDown, cache).map(_.rotateDown)

    if ( cache.contains(key) ) {
      cache(key)
    } else {
      if ( key.mDown == 1 ) {
        cache(key) = rotateRight
        cache(key)
      } else if (key.nRight == 1){
        cache(key) = rotateDown
        cache(key)
      } else {
        cache(key) = rotateDown ++ rotateRight
        cache(key)
      }
    }

  }

  def sumOfMaximalPath(
    key: MnKey,
    cache: mutable.HashMap[MnKey, Set[DicePath]]
  ): Int = {
    val dicePaths = rotateDicePath(key, cache)
    val sum = dicePaths.map(_.sum).max
    sum
  }

  object model {

    case class MnKey(mDown: Int, nRight: Int) {

      def nextRight: MnKey = copy(nRight = nRight -1)

      def nextDown: MnKey = copy(mDown = mDown -1)

    }

    case class DicePath(
      dice: Dice,
      sum: Int,
    ) {

      def rotateDown: DicePath = {
        val nextDice = dice.rotateDown
        val nextSum = sum + nextDice.top
        val nextDicePath = DicePath(nextDice, nextSum)
        nextDicePath
      }

      def rotateRight: DicePath = {
        val nextDice = dice.rotateRight
        val nextSum = sum + nextDice.top
        val nextDicePath = DicePath(nextDice, nextSum)
        nextDicePath
      }

    }

    object Dice {

      lazy val initialDice: Dice =
        Dice(
          top = 1,
          bottom = 6,
          left = 3,
          right = 4,
          front = 2,
          back = 5
        )

    }

    case class Dice(top: Int, bottom: Int, left: Int, right: Int, front: Int, back: Int) {

      def rotateRight: Dice = {
        val nextTop = left
        val nextBottom = right
        val nextLeft = bottom
        val nextRight = top
        val nextFront = front
        val nextBack = back
        Dice(nextTop, nextBottom, nextLeft, nextRight, nextFront, nextBack)
      }

      def rotateDown: Dice = {
        val nextTop = back
        val nextBottom = front
        val nextLeft = left
        val nextRight = right
        val nextFront = top
        val nextBack = bottom
        Dice(nextTop, nextBottom, nextLeft, nextRight, nextFront, nextBack)
      }

    }

  }

}
