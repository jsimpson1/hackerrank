package hackerrank.functionalprogramming.memoizationanddp

import scala.collection.mutable

object SherlockAndTheMaze {

  val cache: mutable.HashMap[PathKey, Set[Path]] = mutable.HashMap[PathKey, Set[Path]](
    (PathKey(1,1,0), Set(Path(List(GridSquare(1,1)), 0))),
  )

  case class GridSquare(row: Int, column: Int) {
    def nextRight: GridSquare = copy(column = column + 1)
    def nextDown: GridSquare = copy(row = row + 1)
  }

  case class Path(
    squares: List[GridSquare],
    numOfChangesInDirection: Int
  ) {

    def currentSquare: GridSquare = squares.head

    private def isInSameDirection(square: SherlockAndTheMaze.GridSquare): Boolean = {

      squares match {
        case _ :: Nil =>
          true
        case cs :: ps :: _ =>
          val movingLeft = cs.row == ps.row
          if ( movingLeft ) {
            square.row == currentSquare.row
          } else {
            square.column == currentSquare.column
          }
      }

    }

    private def moveTo(nextSquare: GridSquare): Path = {

      val isSameDirection = isInSameDirection(nextSquare)

      val nextNumOfChangesInDirection: Int = {

        if ( isSameDirection ) {
          numOfChangesInDirection
        } else {
          numOfChangesInDirection + 1
        }
      }
      val nextSquares = nextSquare :: squares
      Path(nextSquares, nextNumOfChangesInDirection)
    }

    def moveRight: Path = {
      val nextSquare = currentSquare.nextRight
      moveTo(nextSquare)
    }

    def moveDown: Path = {
      val nextSquare = currentSquare.nextDown
      moveTo(nextSquare)
    }

  }

  case class PathKey(row: Int, column: Int, numOfTurns: Int) {
    def nextRight: PathKey = copy(column = column + 1)
    def nextDown: PathKey = copy(row = row + 1)
  }

  def findValidPaths(
    key: PathKey,
    maxRow: Int,
    maxColumn: Int,
    paths: Set[Path]
  ): Set[Path] = {

    lazy val moveDown = findValidPaths(key.nextDown, maxRow, maxColumn, paths.map(_.moveDown))

    lazy val moveRight = findValidPaths(key.nextRight, maxRow, maxColumn, paths.map(_.moveRight))

    if ( key.row == maxRow ) {
      if ( key.column == maxColumn) {
        paths
      } else {
        moveRight
      }
    } else if (key.column == maxColumn ) {
        moveDown
    } else {
      moveRight ++ moveDown
    }

  }

  def calculateNumOfPaths(
    maxRow: Int,
    maxColumn: Int,
    maxNumOfTurns: Int,
  ): Set[Path] = {

    val initialPath = Set(Path(List(GridSquare(1,1)), 0))

    val validPaths: Set[Path] = {
      if ( maxRow == 1 && maxColumn == 1) {
        Set(Path(List(GridSquare(1, 1)), 0))
      } else {
        val turns = (0 to maxNumOfTurns)
        turns
          .foldLeft(Set[Path]()) { (acc, numOfTurn) =>
           val paths = findValidPaths(PathKey(1, 1, numOfTurn), maxRow, maxColumn, initialPath)
            acc ++ paths
          }
      }

    }

    val result = validPaths.filter(_.numOfChangesInDirection <= maxNumOfTurns)
    result
  }

  def solveLine(inputLine: String): Int = {

    val nmk = inputLine.split(" ").map(_.toInt)

    val rows = nmk(0)
    val columns = nmk(1)
    val maxNumOfTurns = nmk(2)

    val numOfWays = calculateNumOfPaths(rows, columns, maxNumOfTurns)

    val result = numOfWays.size
    
    result
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
