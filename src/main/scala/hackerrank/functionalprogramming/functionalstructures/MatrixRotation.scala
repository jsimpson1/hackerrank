package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.data.MatrixRotationInputs

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

      def rotateCellCounterClockwise(cell: Cell, dimensions: HollowSquareDimensions): Cell = {

        val newRowAndColumn =
          (cell.row, cell.column) match {
            case (row, column) if row == dimensions.minRow =>
              if ( column == dimensions.minColumn) (row + 1, column)
              else (row, column - 1 )
            case (row, column) if column == dimensions.minColumn =>
              if ( row == dimensions.maxRow) (row, column + 1)
              else (row + 1, column)
            // bottom right
            case (row, column) if row == dimensions.maxRow =>
              if ( column == dimensions.maxColumn) (row - 1, column)
              else (row, column + 1)
            // top right
            case (row, column) if column == dimensions.maxColumn =>
              if ( row == dimensions.minRow) (row, column - 1)
              else (row - 1, column)
          }
        cell.copy(row = newRowAndColumn._1, column = newRowAndColumn._2)
      }

    }

    case class HollowSquareDimensions(minRow: Int, maxRow: Int, minColumn: Int, maxColumn: Int){
      def isRowMatch(row: Int): Boolean =
        List(minRow, maxRow)
          .contains(row)

      def isColumnMatch(column: Int): Boolean =
        List(minColumn, maxColumn)
          .contains(column)

      def nextSmallerSquare: HollowSquareDimensions =
        HollowSquareDimensions(minRow + 1, maxRow - 1, minColumn + 1, maxColumn - 1)
    }

    case class HollowSquare(cells: IndexedSeq[Cell], dimensions: HollowSquareDimensions) {
      def size: Int = cells.size
    }

    case class Matrix(cells: IndexedSeq[Cell]) {

      lazy val maxRow: Int = cells.maxBy(_.row).row
      lazy val maxColumn: Int = cells.maxBy(_.column).column

      @tailrec
      private def groupCellsByHollowSquarePattern(
        cells: IndexedSeq[Cell],
        dimensions: HollowSquareDimensions,
        result: IndexedSeq[HollowSquare]
      ): IndexedSeq[HollowSquare] = {
        cells match {
          case IndexedSeq() =>
            result
          case values =>
            values
              .partition{ cell =>
                dimensions.isRowMatch(cell.row) || dimensions.isColumnMatch(cell.column)
              } match { case (hollowSquare, remainder) =>
                groupCellsByHollowSquarePattern(
                  remainder,
                  dimensions.nextSmallerSquare,
                  result :+ HollowSquare(hollowSquare, dimensions)
                )
              }
        }
      }

      def rotateCell(initialCell: Cell, dimensions: HollowSquareDimensions, times: Int): Cell = {
        (1 to times).foldLeft(initialCell) { (cell, _) =>
          Matrix.rotateCellCounterClockwise(cell, dimensions)
        }
      }

      def rotate(times: Int): Matrix = {
        Matrix(
          groupCellsByHollowSquarePattern(cells, HollowSquareDimensions(0, maxRow, 0, maxColumn), IndexedSeq())
            .flatMap { hollowSquare =>
              val distinctRotationTimes = times % hollowSquare.size
              hollowSquare.cells.map{ cell =>
                rotateCell(cell, hollowSquare.dimensions, distinctRotationTimes)
              }
            }
        )
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
