package hackerrank.recursion.functionalstructure

import hackerrank.recursion.functionalstructures.{MatrixRotation, MatrixRotationInputs}
import hackerrank.recursion.functionalstructures.MatrixRotation.model._
import org.scalatest.funsuite.AnyFunSuite

class MatrixRotationTest extends AnyFunSuite {

  test("case0 rotate 1") {

    val matrix =
      Matrix(IndexedSeq(
        Cell(1,0,0),
        Cell(2,0,1),
        Cell(3,0,2),
        Cell(4,0,3),
        Cell(5,1,0),
        Cell(6,1,1),
        Cell(7,1,2),
        Cell(8,1,3),
        Cell(9,2,0),
        Cell(10,2,1),
        Cell(11,2,2),
        Cell(12,2,3),
        Cell(13,3,0),
        Cell(14,3,1),
        Cell(15,3,2),
        Cell(16,3,3),
      ))

    val actual = matrix.rotate(1)

    val expected =
      Matrix(IndexedSeq(
        Cell(1,1,0),
        Cell(2,0,0),
        Cell(3,0,1),
        Cell(4,0,2),
        Cell(5,2,0),
        Cell(6,2,1),
        Cell(7,1,1),
        Cell(8,0,3),
        Cell(9,3,0),
        Cell(10,2,2),
        Cell(11,1,2),
        Cell(12,1,3),
        Cell(13,3,1),
        Cell(14,3,2),
        Cell(15,3,3),
        Cell(16,2,3),
      ))

//    actual
//      .cells.sortBy(_.value)
//      .zip(expected.cells.sortBy(_.value))
//      .foreach { v =>
//        assertResult(v._2)(v._1)
//      }

    println(
      s""" actual:\n$actual
         |expected:\n$expected""".stripMargin
    )

    assertResult(expected.sortedCells)(actual.sortedCells)
  }

  test("case0 middle"){

    val matrix =
      Matrix(IndexedSeq(
        Cell(6,0,0),
        Cell(7,0,1),
        Cell(10,1,0),
        Cell(11,1,1),
      ))

    val actual = matrix.rotate(1)

    val expected =
      Matrix(IndexedSeq(
        Cell(6,1,0),
        Cell(7,0,0),
        Cell(10,1,1),
        Cell(11,0,1),
      ))

    actual
      .cells.sortBy(_.value)
      .zip(expected.cells.sortBy(_.value))
      .foreach { v =>
        assertResult(v._2)(v._1)
      }

    println(
      s""" actual:\n$actual
         |expected:\n$expected""".stripMargin
    )

    assertResult(expected.sortedCells)(actual.sortedCells)
  }

  test("case0") {
    val input = MatrixRotation.parseInput(MatrixRotationInputs.case0)
    val actual = input._1.rotate(input._2).toString
    val expected = MatrixRotationInputs.case0Result
    assertResult(expected)(actual)
  }

  test("case1") {
    val input = MatrixRotation.parseInput(MatrixRotationInputs.case1)
    val actual = input._1.rotate(input._2).toString
    val expected = MatrixRotationInputs.case1Result
    assertResult(expected)(actual)
  }

  test("case2") {
    val input = MatrixRotation.parseInput(MatrixRotationInputs.case2)
    val actual = input._1.rotate(input._2).toString
    val expected = MatrixRotationInputs.case2Result
    assertResult(expected)(actual)
  }

  test("case3") {
    val input = MatrixRotation.parseInput(MatrixRotationInputs.case3)
    val actual = input._1.rotate(input._2).toString
    val expected = MatrixRotationInputs.case3Result
    assertResult(expected)(actual)
  }

}
