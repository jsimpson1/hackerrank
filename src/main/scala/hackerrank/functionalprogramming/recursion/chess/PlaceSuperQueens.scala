package hackerrank.functionalprogramming.recursion.chess

import java.util.Date
import java.util.concurrent.TimeUnit
import scala.math.abs

object PlaceSuperQueens {

  def main(args: Array[String]): Unit = {

    val n = 10

    //    singleRecursionDemo(3, n)

    val start = new Date()

    mainDemo(n)
    val end = new Date()

    println(s"time diff = ${getDateDiff(start, end, TimeUnit.SECONDS)}")

  }


  def getDateDiff(date1: Date, date2: Date, timeUnit: TimeUnit): Long = {
    val diffInMillies = date2.getTime - date1.getTime
    timeUnit.convert(diffInMillies, TimeUnit.MILLISECONDS)
  }

  def singleRecursionDemo(startRow: Int, n: Int) = {
    implicit val size: ChessBoardSize = ChessBoardSize(n)
    val result = r(startRow, ChessBoard(), Set()) // works
    println(s"singleRecursionDemo -- result size =${result.size}")
    result.foreach { chessboard =>
      println(chessboard.toString)
      println("\n-------------------------------------\n")
    }
  }

  def mainDemo(n: Int): Unit = {
    val result = placeQueens(n)
    result.zipWithIndex.foreach { case (chessBoard, i) =>
      println(s"case ${i + 1}:")
      println(chessBoard.toString)
    }
    println(s"mainDemo -- size=${result.size}")
  }

  def r(
    row: Int,
    currentChessBoard: ChessBoard,
    closedSquares: Set[ChessBoardSquare],
  )(
    implicit
    size: ChessBoardSize
  ): IndexedSeq[ChessBoard] = {
    val column = currentChessBoard.pieces.size + 1
    if (currentChessBoard.isValidSolution) {
      IndexedSeq(currentChessBoard)
    } else if (row > size.value && !currentChessBoard.pieces.exists(_.column == column)) {
      IndexedSeq()
    } else {
      val potentialQueenSquare = ChessBoardSquare(row, column)
      val isValid = isPlacementValid(currentChessBoard, potentialQueenSquare, closedSquares)
      if (isValid) {
        val newQueen = SuperQueen(potentialQueenSquare)

        if (column == 1) {
          r(
            row = 1,
            currentChessBoard = currentChessBoard.copy(pieces = newQueen :: currentChessBoard.pieces),
            closedSquares ++ newQueen.potentialMoves
          )
        } else {
          r(
            row = 1,
            currentChessBoard = currentChessBoard.copy(pieces = newQueen :: currentChessBoard.pieces),
            closedSquares ++ newQueen.potentialMoves
          ) ++ r(
            row = row + 1,
            currentChessBoard = currentChessBoard,
            closedSquares
          )
        }
      } else {
        r(row + 1, currentChessBoard, closedSquares)
      }
    }
  }

  def isPlacementValid(
    chessboard: ChessBoard,
    square: ChessBoardSquare,
    closedRows: Set[ChessBoardSquare]
  )(implicit size: ChessBoardSize): Boolean = {
    !closedRows.contains(square) &&
      !chessboard
        .pieces
        .exists { piece =>
          lazy val sameRow = piece.row == square.row
          lazy val sameDiagonal = abs(piece.row - square.row) == abs(piece.column - square.column)
          lazy val sameKnight = abs(piece.row - square.row) <= 2 && abs(piece.column - square.column) <= 2
          sameKnight || sameRow || sameDiagonal
        }
  }

  def placeQueens(n: Int): IndexedSeq[ChessBoard] = {

    implicit val size: ChessBoardSize = ChessBoardSize(n)

    (1 to n)
      .flatMap { row =>
        r(row, ChessBoard(), Set())
      }
  }

}
