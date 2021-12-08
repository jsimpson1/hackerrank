package hackerrank.functionalprogramming.memoizationanddp

import scala.collection.{immutable, mutable}

object SherlockAndTheMaze {

  case class GridSquare(row: Int, column: Int) {
    def nextLeft: GridSquare = copy(column = column - 1)
    def nextUp: GridSquare = copy(row = row - 1)
  }

  case class Path(
    squares: List[GridSquare],
    numOfChangesInDirection: Int
  ) {

    def currentSquare: GridSquare = squares.head

    private def moveTo(nextSquare: GridSquare): Path = {

      def isInSameDirection(square: SherlockAndTheMaze.GridSquare): Boolean = {

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

    def moveLeft: Path = {
      val nextSquare = currentSquare.nextLeft
      moveTo(nextSquare)
    }

    def moveUp: Path = {
      val nextSquare = currentSquare.nextUp
      moveTo(nextSquare)
    }

  }

  val topLeft: GridSquare = GridSquare(1,1)

  def findValidPaths(
    key: GridSquare,
    numOfTurns: Int,
    paths: Set[Path]
  ): Set[Path] = {

    lazy val nextLeftPaths =
      paths
        .flatMap{ path =>
          val nextPath = path.moveLeft
          if ( nextPath.numOfChangesInDirection > numOfTurns) {
            None
          } else {
            Some(nextPath)
          }
        }

    lazy val nextUpPaths =
      paths
        .flatMap{ path =>
          val nextPath = path.moveUp
          if (nextPath.numOfChangesInDirection > numOfTurns) {
            None
          } else {
            Some(nextPath)
          }
        }

    lazy val moveLeft = findValidPaths(key.nextLeft, numOfTurns, nextLeftPaths)

    lazy val moveUp = findValidPaths(key.nextUp,numOfTurns, nextUpPaths)

    if ( key == topLeft ) {
      paths
    } else if ( key.column == 1 ){
      val nextPaths = moveUp
      nextPaths
    } else if ( key.row == 1 ){
      val nextPaths = moveLeft
      nextPaths
    } else {
      val nextPaths = moveUp ++ moveLeft
      nextPaths
    }

  }

  def calculateNumOfPaths(
    startingPoint: GridSquare,
    numOfTurns: Int
  ): Set[Path] = {

    val singleSquarePath = Set(Path(List(topLeft), 0))

    val cache: mutable.HashMap[GridSquare, Set[Path]] = mutable.HashMap[GridSquare, Set[Path]](
      (topLeft, singleSquarePath),
    )

    val validPaths = findValidPaths(startingPoint, numOfTurns, Set(Path(List(startingPoint), 0)))

    validPaths
  }

  def solveLine(inputLine: String): Int = {

    val nmk = inputLine.split(" ").map(_.toInt)

    val rows = nmk(0)
    val columns = nmk(1)
    val maxNumOfTurns = nmk(2)


    val numOfWays =
      (0 to maxNumOfTurns).foldLeft(Set[Path]()) { (acc, numOfTurns) =>
        val nextPaths = calculateNumOfPaths(GridSquare(rows,columns), numOfTurns)
        acc ++ nextPaths
      }

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
