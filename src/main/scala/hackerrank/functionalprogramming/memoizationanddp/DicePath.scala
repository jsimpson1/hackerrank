package hackerrank.functionalprogramming.memoizationanddp

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

  private def rotateDicePath(dicePath: DicePath, mDown: Int, nRight: Int, dicePaths: List[DicePath]): List[DicePath] = {

    def doRotation(
      rotationDirection: RotationDirection,
    ): List[DicePath] = {
      rotateDicePath(
        rotationDirection.rotate(dicePath),
        rotationDirection.nextDown(mDown),
        rotationDirection.nextRight(nRight),
        dicePaths
      )
    }

    if (nRight == 1 && mDown == 1) {
      val lastDicePath = dicePath
      val result: List[DicePath] = lastDicePath :: dicePaths
      result
    } else if ( mDown == 1 ) {
      doRotation(RotateRight)
    } else if (nRight == 1){
      doRotation(RotateDown)
    } else {
      val right: List[DicePath] =  doRotation(RotateRight)
      val down: List[DicePath] = doRotation(RotateDown)
      val nextPaths: List[DicePath] = down ::: right
      nextPaths
    }

  }

  def sumOfMaximalPath(mDown: Int, nRight: Int): Int = {
    val dicePaths: List[DicePath] = rotateDicePath(initialDicePath, mDown, nRight, Nil)
    val sum = dicePaths.map(_.sum).max
    sum
  }

  object model {

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
