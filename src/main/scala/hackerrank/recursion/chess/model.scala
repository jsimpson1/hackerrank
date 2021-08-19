package hackerrank.recursion.chess

object model {

  case class ChessBoardSize(value: Int)

  case class ChessBoard(
    pieces: List[SuperQueen] = List(),
  )(
    implicit size: ChessBoardSize
  ) {

    lazy val squares: List[ChessBoardSquare] =
      (1 to size.value).flatMap{ row =>
        (1 to size.value).map { column =>
          ChessBoardSquare(row, column)
        }
      }.toList
      .sorted

    override def toString: String =
      squares
        .groupBy(_.row)
        .toList
        .sortBy(_._1)
        .map{ v: (Int, List[ChessBoardSquare]) =>
          v._2.sorted.map( square =>
            if ( pieces.map(_.position).contains(square)) "1" else "0"
          )
        }.map(row =>
          row.mkString(" ")
        ).mkString("\n")

    def isValidSolution: Boolean =
      pieces.size == size.value

  }

  case class ChessBoardSquare(row: Int, column: Int) extends Ordered[ChessBoardSquare] {
    override def compare(that: ChessBoardSquare): Int = {
      row
        .compare(that.row) match {
        case 0 =>
          column.compare(that.column)
        case o => o
      }
    }

    override def toString: String = s"($row,$column)"
  }

  trait HasPosition {
    implicit def size: ChessBoardSize
    def position: ChessBoardSquare
    def row: Int = position.row
    def column: Int = position.column
  }

  trait DiagonalMove extends HasPosition {
    private def diagonal(verticalModifier: Int, horizontalModifier: Int): IndexedSeq[ChessBoardSquare] = {
      val verticalLimit = if ( verticalModifier > 0) size.value else 1

      val horizontalLimit = if ( horizontalModifier > 0) size.value else 1

      val vertical = row + verticalModifier to verticalLimit by verticalModifier
      val horizontal = column + horizontalModifier to horizontalLimit by horizontalModifier

      vertical
        .zip(horizontal)
        .map(v =>
          ChessBoardSquare(v._1, v._2)
        )
    }

    def diagonalUpRight: IndexedSeq[ChessBoardSquare] =
      diagonal(-1, 1)

    def diagonalDownRight: IndexedSeq[ChessBoardSquare] =
      diagonal(1, 1)

    def diagonalUpLeft: IndexedSeq[ChessBoardSquare] =
      diagonal(-1, -1)

    def diagonalDownLeft: IndexedSeq[ChessBoardSquare] =
      diagonal(1, -1)

    def allDiagonalMoves: IndexedSeq[ChessBoardSquare] =
      diagonalUpRight ++ diagonalDownRight ++ diagonalUpLeft ++ diagonalDownLeft

    def diagonalMoves: IndexedSeq[ChessBoardSquare] =
      diagonalUpRight ++ diagonalDownRight
  }

  trait VerticalMove extends HasPosition {

    def verticalUp: IndexedSeq[ChessBoardSquare] =
      (position.row - 1 to 1 by -1)
        .map( row =>
          ChessBoardSquare(row, column)
        )

    def verticalDown: IndexedSeq[ChessBoardSquare] =
      (position.row + 1 to size.value)
        .map( row =>
          ChessBoardSquare(row, column)
        )

    def verticalMoves: IndexedSeq[ChessBoardSquare] =
      verticalUp ++ verticalDown
  }

  trait HorizontalMove extends HasPosition {

    def horizontalLeft: IndexedSeq[ChessBoardSquare] =
      (position.column - 1 to 1 by -1)
        .map( column =>
          ChessBoardSquare(row, column)
        )

    def horizontalRight: IndexedSeq[ChessBoardSquare] =
      (position.column + 1 to size.value)
        .map( column =>
          ChessBoardSquare(row, column)
        )

    def horizontalMoves: IndexedSeq[ChessBoardSquare] =
      horizontalLeft ++ horizontalRight

  }


  trait KnightMoves extends HasPosition {

    private def knightMove(verticalModifier: Int => Int, horizontalModifier: Int => Int): Option[ChessBoardSquare] = {
      val knightRow: Int = verticalModifier(row)
      val knightColumn: Int = horizontalModifier(column)
      val validVertical = knightRow >= 1 && knightRow <= size.value
      val validHorizontal = knightColumn >= 1 && knightColumn <= size.value
      if ( validVertical && validHorizontal) {
        Some(ChessBoardSquare(knightRow, knightColumn))
      } else {
        None
      }
    }

    def knightUpRightMove: Option[ChessBoardSquare] =
      knightMove(_ - 2, _ + 1)
    def knightUpLeftMove: Option[ChessBoardSquare] =
      knightMove(_ - 2, _ - 1)
    def knightRightUpMove: Option[ChessBoardSquare] =
      knightMove(_ - 1, _ + 2)
    def knightRightDownMove: Option[ChessBoardSquare] =
      knightMove(_ + 1, _ + 2)
    def knightDownRightMove: Option[ChessBoardSquare] =
      knightMove(_ + 2, _ + 1)
    def knightDownLeftMove: Option[ChessBoardSquare] =
      knightMove(_ + 2, _ - 1)
    def knightLeftDownMove: Option[ChessBoardSquare] =
      knightMove(_ + 1, _ - 2)
    def knightLeftUpMove: Option[ChessBoardSquare] =
      knightMove(_ - 1, _ - 2)

    def knightMoves: IndexedSeq[ChessBoardSquare] =
      IndexedSeq(
        knightUpRightMove, knightUpLeftMove,
        knightRightUpMove, knightRightDownMove,
        knightDownRightMove, knightDownLeftMove,
        knightLeftDownMove, knightLeftUpMove
      ).flatten

    def knightMovesToLeft: IndexedSeq[ChessBoardSquare] =
      IndexedSeq(
        knightUpLeftMove, knightDownLeftMove, knightLeftDownMove, knightLeftUpMove
      ).flatten

  }

  trait ChessPiece extends HasPosition {
    def potentialMoves: IndexedSeq[ChessBoardSquare]
  }

  object SuperQueen {

    def apply(row: Int, column: Int)(implicit size: ChessBoardSize): SuperQueen =
      SuperQueen(ChessBoardSquare(row, column))

  }

  case class SuperQueen(
    position: ChessBoardSquare
  )(
    override implicit
    val size: ChessBoardSize
  ) extends Ordered[SuperQueen] with ChessPiece with HorizontalMove with VerticalMove with DiagonalMove with KnightMoves {

    def knightMovesToRight: IndexedSeq[ChessBoardSquare] =
      IndexedSeq(
        knightUpRightMove,
        knightRightUpMove,
        knightRightDownMove,
        knightDownRightMove
      ).flatten

    override def potentialMoves: IndexedSeq[ChessBoardSquare] =
      horizontalRight ++
      diagonalUpRight ++
      diagonalDownRight ++
      knightMovesToRight

    override def compare(that: SuperQueen): Int =
      this.position.compare(that.position)
  }

}
