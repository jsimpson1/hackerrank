package hackerrank.functionalprogramming.memoizationanddp

object SherlockAndTheMaze {

  case class Square(row: Int, column: Int)

  trait MoveDirection {
    def rowIncrement:Int
    def columnIncrement:Int
  }

  case object MoveDown extends MoveDirection {
    override def rowIncrement: Int = 1
    override def columnIncrement: Int = 0
  }

  case object MoveRight extends MoveDirection{
    override def rowIncrement: Int = 0
    override def columnIncrement: Int = 1
  }

  case class Path(
    square: Square,
    previous: Option[Square],
    numOfChangesInDirection: Int
  ) {

    private def move(md: MoveDirection): Path = {
      val nextSquare =
        square
          .copy(
            row = square.row + md.rowIncrement,
            column = square.column + md.columnIncrement
          )
      val nextPrevious: Option[Square] = Some(square)

      val nextNumOfChangesInDirection: Int = {

        def nextDirectionChange(current: Int, previous: Int): Int = {
          if ( current - previous > 0 ) {
            numOfChangesInDirection
          } else {
            numOfChangesInDirection + 1
          }
        }

        previous match {
          case None => 0
          case Some(p) =>
            md match {
              case MoveRight =>
                nextDirectionChange(square.column, p.column)
              case MoveDown =>
                nextDirectionChange(square.row, p.row)
            }
        }
      }
      Path(nextSquare, nextPrevious, nextNumOfChangesInDirection)
    }

    def moveRight: Path = move(MoveRight)

    def moveDown: Path = move(MoveDown)

  }

  def calculateNumOfPaths(rows: Int, columns: Int, maxNumOfTurns: Int): Int = {

    def findValidPaths(row: Int, column: Int, paths: List[Path]): List[Path] = {

      def movePathsRight: List[Path] = {
        val nextPaths = paths
          .flatMap{ path =>
            val nextPath = path.moveRight
            if (nextPath.numOfChangesInDirection > maxNumOfTurns ) {
              None
            } else {
              Some(nextPath)
            }
          }
        findValidPaths(row, column + 1, nextPaths)
      }

      def movePathsDown: List[Path] = {
        val nextPaths = paths
          .flatMap{ path =>
            val nextPath = path.moveDown
            if (nextPath.numOfChangesInDirection > maxNumOfTurns ) {
              None
            } else {
              Some(nextPath)
            }
          }
        findValidPaths(row + 1, column, nextPaths)
      }

      if ( row == rows && column == columns ) {
        paths
      } else if ( row == rows ) {
        movePathsRight
      } else if ( column == columns) {
        movePathsDown
      } else {
        movePathsRight ++ movePathsDown
      }
    }

    val validPaths = findValidPaths(1, 1, List(Path(Square(1, 1), None, 0)))

    val result = validPaths.size

    result
  }

  def solveLine(inputLine: String): Int = {

    val nmk = inputLine.split(" ").map(_.toInt)

    val rows = nmk(0)
    val columns = nmk(1)
    val maxNumOfTurns = nmk(2)

    val numOfWays = calculateNumOfPaths(rows,columns,maxNumOfTurns)

    numOfWays
  }

  def solve(inputStr: String): Unit = {
    inputStr
      .split("\n")
      .tail
      .map(line =>
        solveLine(line)
      ).foreach(
        println
      )
  }

  def solve(): Unit = {
    val numOfCases = readInt
    List
      .range(0, numOfCases)
      .foreach{ _ =>
        val line = readLine
        println(solveLine(line))
      }
  }

}
