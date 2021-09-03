package hackerrank.recursion.functionalstructures

import scala.annotation.tailrec

object MatrixRotation {

  import model._

  def main(args: Array[String]): Unit = {

    val input = parseInput(MatrixRotationInputs.case0)

    val initialMatrix = input._1
    val numOfRotation = input._2

    println(s"initialMatrix:\n${initialMatrix}")

    val rotatedMatrix = initialMatrix.rotate(numOfRotation)

    println(s"rotatedMatrix:\n${rotatedMatrix}")

  }

  def parseInput(input: String): (Matrix, Int) = {
    val lines = input.split("\n")
    val mnrValues = lines.head.split(" ").map(_.toInt)
    val numOfRows = mnrValues(0)
//    val numOfColumns = mnrValues(1)
    val numOfRotations = mnrValues(2)

    val rowValues: IndexedSeq[(IndexedSeq[(Int, Int)], Int)] =
      (1 to numOfRows)
        .map { row =>
          lines(row)
            .split(" ")
            .map(_.toInt)
            .zipWithIndex
            .toIndexedSeq
        }.zipWithIndex

    val cells: IndexedSeq[Cell] =
      rowValues
        .flatMap{ row =>
          val rowIndex = row._2
          row._1.map{ column =>
            val columnIndex = column._2
            Cell(column._1, rowIndex, columnIndex)
          }
        }

    (Matrix(cells), numOfRotations)
  }

  object model {

    case class Cell(value: Int, row: Int, column: Int) extends Ordered[Cell] {

      override def compare(that: Cell): Int =
        column.compare(that.column) match {
          case 0 =>
            row.compare(that.row)
          case v => v
        }

    }

    object Matrix {

      def rotateCellCounterClockwise(cell: Cell, minRow: Int, maxRow: Int, minColumn:Int,  maxColumn: Int): Cell = {

        val newRowAndColumn =
          (cell.row, cell.column) match {
            case (row, column) if row == minRow =>
              if ( column == minColumn) (row + 1, column)
              else (row, column - 1 )
            case (row, column) if column == minColumn =>
              if ( row == maxRow) (row, column + 1)
              else (row + 1, column)
            // bottom right
            case (row, column) if row == maxRow =>
              if ( column == maxColumn) (row - 1, column)
              else (row, column + 1)
            // top right
            case (row, column) if column == maxColumn =>
              if ( row == minRow) (row, column - 1)
              else (row - 1, column)
          }
        cell.copy(row = newRowAndColumn._1, column = newRowAndColumn._2)
      }

    }

    case class Matrix(cells: IndexedSeq[Cell]) {

      lazy val maxRow: Int = cells.maxBy(_.row).row
      lazy val maxColumn: Int = cells.maxBy(_.column).column

      def rotateCounterClockwise: Matrix = {
        @tailrec
        def r(cells: IndexedSeq[Cell], minRow: Int, maxRow: Int, minColumn: Int, maxColumn: Int, result: IndexedSeq[Cell]): IndexedSeq[Cell] = {
          cells match {
            case IndexedSeq() =>
              result
            case values =>
              val hollowSquareOfCellsAndOthers =
                values
                  .partition{ cell =>
                    List(minRow, maxRow).contains(cell.row) || List(minColumn, maxColumn).contains(cell.column)
                  }

              val hollowSquareOfCells = hollowSquareOfCellsAndOthers._1
              val others = hollowSquareOfCellsAndOthers._2

              val rotatedCells =
                hollowSquareOfCells
                  .map(cell =>
                    Matrix
                      .rotateCellCounterClockwise(cell, minRow, maxRow, minColumn, maxColumn)
                  )
              r(others, minRow + 1, maxRow - 1, minColumn + 1, maxColumn -1, result ++ rotatedCells)
          }
        }
        Matrix(
          r(cells,0, maxRow, 0, maxColumn, IndexedSeq())
        )
      }

      def rotate(times: Int): Matrix = {
        (1 to times).foldLeft(this){ (matrix, _) =>
          matrix.rotateCounterClockwise
        }
      }

      def sortedCells: IndexedSeq[Cell] = cells.sorted

      override def toString: String = {
        cells
          .groupBy(_.row)
          .toIndexedSeq
          .sortBy(_._1).map { row =>
            row._2.sorted.map(_.value).mkString(" ")
          }.mkString("\n")
      }
    }

  }

}
