package hackerrank.recursion.chess

import hackerrank.recursion.chess.model._
import org.scalatest.funsuite.AnyFunSuite

import scala.math.Ordering.Implicits.seqDerivedOrdering

class SuperQueenTest extends AnyFunSuite {

  implicit val chessBoardRows: ChessBoardSize = ChessBoardSize(10)

  test("SuperQueen verticalMoves") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual = queen.verticalMoves.toList.sorted
    val expected = List(
      ChessBoardSquare(1,5),
      ChessBoardSquare(2,5),
      ChessBoardSquare(3,5),
      ChessBoardSquare(4,5),
      ChessBoardSquare(6,5),
      ChessBoardSquare(7,5),
      ChessBoardSquare(8,5),
      ChessBoardSquare(9,5),
      ChessBoardSquare(10,5),
    )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen horizontalMoves") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual = queen.horizontalMoves.toList.sorted
    val expected = List(
      ChessBoardSquare(5,1),
      ChessBoardSquare(5,2),
      ChessBoardSquare(5,3),
      ChessBoardSquare(5,4),
      ChessBoardSquare(5,6),
      ChessBoardSquare(5,7),
      ChessBoardSquare(5,8),
      ChessBoardSquare(5,9),
      ChessBoardSquare(5,10),
    )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen diagonalUpRight") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual: IndexedSeq[ChessBoardSquare] = queen.diagonalUpRight.sorted
    val expected = IndexedSeq(
      ChessBoardSquare(4,6),
      ChessBoardSquare(3,7),
      ChessBoardSquare(2,8),
      ChessBoardSquare(1,9),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen diagonalDownRight") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual: IndexedSeq[ChessBoardSquare] = queen.diagonalDownRight.sorted
    val expected = IndexedSeq(
      ChessBoardSquare(6,6),
      ChessBoardSquare(7,7),
      ChessBoardSquare(8, 8),
      ChessBoardSquare(9,9),
      ChessBoardSquare(10,10),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen diagonalUpLeft") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual: IndexedSeq[ChessBoardSquare] = queen.diagonalUpLeft.sorted
    val expected = IndexedSeq(
      ChessBoardSquare(4,4),
      ChessBoardSquare(3,3),
      ChessBoardSquare(2,2),
      ChessBoardSquare(1,1),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen diagonalDownLeft") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual: IndexedSeq[ChessBoardSquare] = queen.diagonalDownLeft.sorted
    val expected = IndexedSeq(
      ChessBoardSquare(6,4),
      ChessBoardSquare(7,3),
      ChessBoardSquare(8,2),
      ChessBoardSquare(9,1),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen diagonalMoves from corner") {
    val queen = SuperQueen(ChessBoardSquare(1, 1))
    val actual = queen.diagonalMoves.toList.sorted
    val expected = List(
      ChessBoardSquare(2,2),
      ChessBoardSquare(3,3),
      ChessBoardSquare(4,4),
      ChessBoardSquare(5,5),
      ChessBoardSquare(6,6),
      ChessBoardSquare(7,7),
      ChessBoardSquare(8,8),
      ChessBoardSquare(9,9),
      ChessBoardSquare(10,10),
    )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen knightMoves") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual = queen.knightMoves.toList.sorted
    val expected = List(
      ChessBoardSquare(3,4),
      ChessBoardSquare(3,6),
      ChessBoardSquare(4,3),
      ChessBoardSquare(4,7),
      ChessBoardSquare(6,3),
      ChessBoardSquare(6,7),
      ChessBoardSquare(7,4),
      ChessBoardSquare(7,6),
    )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen knightMovesToRight") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual = queen.knightMovesToRight.toList.sorted
    val expected = List(
      ChessBoardSquare(3,6),
      ChessBoardSquare(4,7),
      ChessBoardSquare(6,7),
      ChessBoardSquare(7,6),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen knightMovesToRight near right edge of board") {
    val queen = SuperQueen(ChessBoardSquare(5, 9))
    val actual = queen.knightMovesToRight.toList.sorted
    val expected = List(
      ChessBoardSquare(3,10),
      ChessBoardSquare(7,10),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("SuperQueen potentialMoves !! currently Only to right !!") {
    val queen = SuperQueen(ChessBoardSquare(5, 5))
    val actual = queen.potentialMoves.toList.sorted
    val expected = List(
      // horizontal
      ChessBoardSquare(5,6),
      ChessBoardSquare(5,7),
      ChessBoardSquare(5,8),
      ChessBoardSquare(5,9),
      ChessBoardSquare(5,10),
      // diagonalUpRight
      ChessBoardSquare(4,6),
      ChessBoardSquare(3,7),
      ChessBoardSquare(2,8),
      ChessBoardSquare(1,9),
      // diagonalDownRight
      ChessBoardSquare(6,6),
      ChessBoardSquare(7,7),
      ChessBoardSquare(8,8),
      ChessBoardSquare(9,9),
      ChessBoardSquare(10,10),
      // knightMoves
      ChessBoardSquare(3,6),
      ChessBoardSquare(4,7),
      ChessBoardSquare(6,7),
      ChessBoardSquare(7,6),
    ).sorted
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  test("place queens n = 10 with pieces listed"){
    val actual: IndexedSeq[List[ChessPiece]] =
      PlaceSuperQueens
        .placeQueens(10)
        .map(_.pieces.sorted)
        .sorted

    val expected: IndexedSeq[List[ChessPiece]] = IndexedSeq(
      /*
0 0 0 1 0 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 1 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 0 0 0 1
0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 0 1 0 0 0
       */
      List(
        SuperQueen(1,3),
        SuperQueen(2,6),
        SuperQueen(3,9),
        SuperQueen(4,1),
        SuperQueen(5,4),
        SuperQueen(6,7),
        SuperQueen(7,10),
        SuperQueen(8,2),
        SuperQueen(9,5),
        SuperQueen(10,8)
      ),
      /*
0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 0 0 1 0
1 0 0 0 0 0 0 0 0 0
0 0 0 1 0 0 0 0 0 0
0 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0 0 1
0 1 0 0 0 0 0 0 0 0
0 0 0 0 1 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0
       */
      List(
        SuperQueen(1,4),
        SuperQueen(2,8),
        SuperQueen(3,1),
        SuperQueen(4,5),
        SuperQueen(5,9),
        SuperQueen(6,2),
        SuperQueen(7,6),
        SuperQueen(8,10),
        SuperQueen(9,3),
        SuperQueen(10,7),
      ),
      /*
0 0 0 0 0 0 0 1 0 0
0 0 0 0 1 0 0 0 0 0
0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 1
0 0 0 0 0 0 1 0 0 0
0 0 0 1 0 0 0 0 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 0 0 0 0 1 0 0 0 0
0 0 1 0 0 0 0 0 0 0
       */
      List(
        SuperQueen(1,7),
        SuperQueen(2,3),
        SuperQueen(3,10),
        SuperQueen(4,6),
        SuperQueen(5,2),
        SuperQueen(6,9),
        SuperQueen(7,5),
        SuperQueen(8,1),
        SuperQueen(9,8),
        SuperQueen(10,4),
      ),
      /*
0 0 0 0 0 0 1 0 0 0
0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 1
0 0 0 0 0 1 0 0 0 0
0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 0 0 0 1 0 0 0 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0
0 0 0 1 0 0 0 0 0 0
       */
      List(
        SuperQueen(1,8),
        SuperQueen(2,5),
        SuperQueen(3,2),
        SuperQueen(4,10),
        SuperQueen(5,7),
        SuperQueen(6,4),
        SuperQueen(7,1),
        SuperQueen(8,9),
        SuperQueen(9,6),
        SuperQueen(10,3),
      )
    )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)

  }

  test("place queens n = 11"){
    val actual = PlaceSuperQueens.placeQueens(11).size

    val expected: Int = 44

    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }

  ignore("place queens n = 14"){
    val actual = PlaceSuperQueens.placeQueens(14).size

    val expected: Int = 5180

    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    assertResult(expected)(actual)
  }



}
