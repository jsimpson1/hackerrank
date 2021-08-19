package hackerrank.recursion.chess

import hackerrank.recursion.chess.model.{DiagonalMove, _}

import java.util.Date
import java.util.concurrent.TimeUnit

object PlaceSuperQueens {

  def main(args: Array[String]): Unit = {

    val n = 13

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
    val result = r(startRow, ChessBoard()).flatten // works
    println(s"singleRecursionDemo -- result size =${result.size}")
    result.foreach{ chessboard =>
      println(chessboard.toString)
      println("\n-------------------------------------\n")
    }
  }

  def mainDemo(n: Int): Unit = {
    val result = placeQueens(n)
    result.zipWithIndex.foreach{ case(chessBoard, i) =>
      println(s"case ${i + 1}:")
      println(chessBoard.toString)
    }
    println(s"mainDemo -- size=${result.size}")
  }

  def r(
    row: Int,
    currentChessBoard: ChessBoard,
  )(
    implicit
      size: ChessBoardSize
  ): IndexedSeq[Option[ChessBoard]] = {
    val column = currentChessBoard.pieces.size + 1
    if ( currentChessBoard.pieces.size == size.value ) {
      IndexedSeq(Some(currentChessBoard))
    } else {
      if ( row > size.value) {
        currentChessBoard
          .pieces match {
            case Nil =>
              IndexedSeq(None)
            case pieces =>
              if ( !currentChessBoard.isValidSolution ) {
                IndexedSeq(None)
              } else {
                val head = pieces.head
                val tail = pieces.tail
                r( head.row + 1, currentChessBoard.copy(pieces = tail))
              }
          }
      } else {
        val potentialQueenSquare = ChessBoardSquare(row, column)
        val isValid = isPlacementValid(currentChessBoard, potentialQueenSquare)
        if ( isValid ) {
          val newQueen = SuperQueen(potentialQueenSquare)
          r(
            row = 1,
            currentChessBoard = currentChessBoard.copy(pieces = newQueen :: currentChessBoard.pieces),
          ) ++ r(
            row = row + 1,
            currentChessBoard = currentChessBoard,
          )
        } else {
          r(row + 1, currentChessBoard)
        }
      }
    }
  }

  def isPlacementValid(
    chessboard: ChessBoard,
    square: ChessBoardSquare,
  )(implicit size: ChessBoardSize): Boolean = {
    !chessboard
      .pieces
        .exists { piece =>
          lazy val sameRow = piece.horizontalRight.contains(square)
          lazy val sameDiagonal = piece.diagonalDownRight.contains(square) || piece.diagonalUpRight.contains(square)
          lazy val sameKnight = piece.knightMovesToRight.contains(square)
          sameRow || sameDiagonal || sameKnight
        }
  }

  def placeQueens(n: Int): IndexedSeq[ChessBoard] = {

    implicit val size: ChessBoardSize = ChessBoardSize(n)

    (1 to n)
      .flatMap { row =>
        r(row, ChessBoard())
      }.flatten
      .distinct
  }

}
