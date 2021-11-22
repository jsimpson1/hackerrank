package hackerrank.functionalprogramming.memoizationanddp

import scala.collection.immutable.HashMap


sealed trait RotationDirection {
  def idChar: Char
  def rotate(dicePath: DicePath): DicePath
  def nextRight(i: Int): Int
  def nextDown(i: Int): Int
}

case object RotateRight extends RotationDirection {
  def idChar: Char = 'r'
  override def rotate(dicePath: DicePath): DicePath = dicePath.rotateRight
  override def nextRight(i: Int): Int = i - 1
  override def nextDown(i: Int): Int = i
}

case object RotateDown extends RotationDirection {
  override def idChar: Char = 'd'
  override def rotate(dicePath: DicePath): DicePath = dicePath.rotateDown
  override def nextRight(i: Int): Int = i
  override def nextDown(i: Int): Int = i - 1
}

object DicePath {

  lazy val initialDicePath: DicePath = DicePath("", Dice.initialDice, 1)

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
    val cases: List[(Int, Int)] =
      inputStr
        .split("\n")
        .tail
        .map{ line =>
          line
            .split(" ")
            .map(_.toInt) match {
              case Array(m, n) =>
                (m, n)
            }
        }.toList
    val result = cases.map(c => sumOfMaximalPath(c._1, c._2))
    result.foreach(println)
  }

  private def rotateDicePath(dicePath: DicePath, numOfRight: Int, numOfDown: Int, dicePaths: List[DicePath]): List[DicePath] = {

    def doRotation(
      rotationDirection: RotationDirection,
    ): List[DicePath] = {
      rotateDicePath(
        rotationDirection.rotate(dicePath),
        rotationDirection.nextRight(numOfRight),
        rotationDirection.nextDown(numOfDown),
        dicePaths
      )
    }

    if (numOfRight == 1 && numOfDown == 1) {
      val lastDicePath = dicePath
      val result: List[DicePath] = lastDicePath :: dicePaths
      result
    } else if ( numOfDown == 1 ) {
      doRotation(RotateRight)
    } else if (numOfRight == 1){
      doRotation(RotateDown)
    } else {
      val right: List[DicePath] =  doRotation(RotateRight)
      val down: List[DicePath] = doRotation(RotateDown)
      val nextPaths: List[DicePath] = down ::: right
      nextPaths
    }

  }

  def sumOfMaximalPath(movesDown: Int, movesRight: Int): Int = {
    val dicePaths: List[DicePath] = rotateDicePath(initialDicePath, movesRight, movesDown, Nil)
    val sum = dicePaths.map(_.sum).max
    sum
  }

}

case class DicePath(
  pathId: String,
  dice: Dice,
  sum: Int,
) {

  def rotateDown: DicePath = {
    val nextDice = dice.rotateDown
    val nextSum = sum + nextDice.top
    val nextPathId = RotateDown.idChar + pathId
    val nextDicePath = DicePath(nextPathId, nextDice, nextSum)
    nextDicePath
  }

  def rotateRight: DicePath = {
    val nextDice = dice.rotateRight
    val nextSum = sum + nextDice.top
    val nextPathId = RotateRight.idChar + pathId
    val nextDicePath = DicePath(nextPathId, nextDice, nextSum)
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
