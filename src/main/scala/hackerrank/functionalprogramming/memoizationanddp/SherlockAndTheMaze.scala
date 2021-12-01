package hackerrank.functionalprogramming.memoizationanddp

object SherlockAndTheMaze {

  trait PathDirection

  case object MoveRight extends PathDirection

  case object MoveDown extends PathDirection

  case class Path(
    directions: List[PathDirection],
    lastDirectionChange: Option[PathDirection],
    numOfChangesInDirection: Int
  ) {

    private def turn(pathDirection: PathDirection): Path = {
      val nextDirections = pathDirection :: directions
      lastDirectionChange match {
        case None =>
          copy(nextDirections, Some(pathDirection), 0)
        case Some(ldc) =>
          if ( ldc == pathDirection ) {
            copy(directions = nextDirections)
          } else {
            copy(directions = nextDirections, Some(pathDirection), numOfChangesInDirection + 1)
          }
      }
    }

    def turnRight: Path = turn(MoveRight)

    def turnDown: Path = turn(MoveDown)


  }

  def calculateNumOfPaths(rows: Int, columns: Int, maxNumOfTurns: Int): Int = {

    def nextMove(path: Path, direction: PathDirection): Option[Path] = {
      direction match {
        case MoveRight =>
          Some(path.turnRight)
        case MoveDown =>
          Some(path.turnDown)
      }
    }

    def findValidPaths(row: Int, column: Int, paths: List[Path]): List[Path] = {

      def move(direction: PathDirection): List[Path] = {
        val nextPaths =
          paths.flatMap{ path =>

            def move: Option[Path] = nextMove(path, direction)

            path.lastDirectionChange match {
              case None =>
                move
              case Some(ldc) =>
                if ( ldc == direction ) {
                  move
                } else {
                  if ( path.numOfChangesInDirection < maxNumOfTurns) {
                    move
                  } else {
                    None
                  }
                }
            }

          }
        if (nextPaths.isEmpty) {
          Nil
        } else {
          val nextRow = if ( direction == MoveDown ) row + 1 else row
          val nextColumn = if ( direction == MoveRight ) column + 1 else column
          findValidPaths(nextRow, nextColumn, nextPaths)
        }
      }

      def moveRight: List[Path] = move(MoveRight)

      def moveDown: List[Path] = move(MoveDown)

      if ( row == rows && column == columns ) {
        paths
      } else if ( row == rows ) {
        moveRight
      } else if ( column == columns) {
        moveDown
      } else {
        moveRight ++ moveDown
      }
    }

    val validPaths = findValidPaths(1, 1, List(Path(Nil, None, 0)))

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
      .foreach{ line =>

      }
  }

}
