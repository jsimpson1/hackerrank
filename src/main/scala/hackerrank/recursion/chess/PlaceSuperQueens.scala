package hackerrank.recursion.chess

import hackerrank.recursion.chess.model._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.Stack

object PlaceSuperQueens {

  def main(args: Array[String]): Unit = {

    val n = 11

//    singleRecursionDemo(3, n)

    mainDemo(n)

  }

  def singleRecursionDemo(startRow: Int, n: Int) = {
    implicit val size: ChessBoardSize = ChessBoardSize(n)
    val result: IndexedSeq[ChessBoard] = r(startRow, ChessBoard()).flatten // works
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
              val isInvalidSolution: Boolean = !currentChessBoard.isValidSolution || currentChessBoard.pieces.exists(_.column == column)
              if ( isInvalidSolution ) {
                IndexedSeq(None)
              } else {
                r( pieces.head.row + 1, currentChessBoard.copy(pieces = pieces.tail))
              }
          }
      } else {
        val potentialQueenSquare = ChessBoardSquare(row, column)
        val isValid = isPlacementValid(currentChessBoard, potentialQueenSquare)
        if ( isValid ) {
          val newPieces: List[ChessPiece] = SuperQueen(potentialQueenSquare) :: currentChessBoard.pieces
          r(1, currentChessBoard.copy(pieces = newPieces)) ++ r(row + 1, currentChessBoard)
        } else {
          r(row + 1, currentChessBoard)
        }
      }
    }
  }

  def isPlacementValid(chessboard: ChessBoard, square: ChessBoardSquare): Boolean = {
    !chessboard
      .pieces
      .exists { piece =>
        piece.potentialMoves.contains(square)
      }
  }

  def placeQueens(n: Int): IndexedSeq[ChessBoard] = {

    implicit val size: ChessBoardSize = ChessBoardSize(n)

    val result: IndexedSeq[ChessBoard] =
    (1 to n)
      .flatMap { row =>
        r(row, ChessBoard())
      }.flatten
      .distinct

    result
  }

}
