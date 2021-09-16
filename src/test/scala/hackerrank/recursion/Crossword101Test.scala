package hackerrank.recursion

import hackerrank.recursion.Crosswords101.model._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.JavaConverters._

import scala.math.Ordering.Implicits.seqDerivedOrdering

class Crossword101Test extends AnyFunSuite {

  test("CrosswordSquare -- letter match") {
    assertResult(true)(Letter('a', 0,0) == Letter('a', 0, 0))
  }

  test("CrosswordSquare -- same coordinates") {
    assertResult(true)(Open(0,0).hasSameCoordinates(Closed(0,0)))
  }

  test("CrosswordSquare -- different coordinates") {
    assertResult(false)(Open(0,0).hasSameCoordinates(Closed(1,1)))
  }

  test("CrosswordSquare -- isClosed") {
    assertResult(true)(Closed(0,0).isClosed)
  }

  test("CrosswordSquare -- isOpen") {
    assertResult(true)(Open(0,0).isOpen)
  }

  test("CrosswordSquare -- isLetter") {
    assertResult(true)(Letter('a', 0,0).isLetter)
  }

  test("CrosswordLine -- resolveWordOpenings no opening") {
    val line = CrosswordLine(
      List(
        Closed(0,0),
        Closed(0,1),
        Closed(0,2),
        Closed(0,3),
        Closed(0,4),
        Closed(0,5),
        Closed(0,6),
        Closed(0,7),
        Closed(0,8),
        Closed(0,9),
      )
    )
    val actual: List[CrosswordLine] =
      line
        .allWordOpenings
        .map(line =>
          line
            .copy(
              squares = line.squares.sortBy(_.coordinate.columnIndex)
            )
        )
    val expected: List[CrosswordLine] = Nil
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    println("---------------------------")
    assertResult(expected)(actual)
  }

  test("CrosswordLine -- resolveWordOpenings one opening") {
    val line = CrosswordLine(
      List(
        Closed(0,0),
        Open(0,1),
        Open(0,2),
        Open(0,3),
        Open(0,4),
        Closed(0,5),
        Closed(0,6),
        Closed(0,7),
        Closed(0,8),
        Closed(0,9),
      )
    )
    val actual: List[CrosswordLine] =
      line
        .allWordOpenings
        .map(line =>
          line
            .copy(
              squares = line.squares.sortBy(_.coordinate.columnIndex)
            )
        )
    val expected: List[CrosswordLine] =
      List(
        CrosswordLine(
          List(
            Open(0,1),
            Open(0,2),
            Open(0,3),
            Open(0,4),
          )
        )
      )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    println("---------------------------")
    assertResult(expected)(actual)
  }

  test("CrosswordLine -- resolveWordOpenings two opening") {
    val line = CrosswordLine(
      List(
        Closed(0,0),
        Open(0,1),
        Open(0,2),
        Open(0,3),
        Open(0,4),
        Closed(0,5),
        Open(0,6),
        Open(0,7),
        Open(0,8),
        Closed(0,9),
      )
    )
    val actual: List[CrosswordLine] =
      line
        .allWordOpenings
        .map(line =>
          line
            .copy(
              squares = line.squares.sortBy(_.coordinate.columnIndex)
            )
        ).reverse
    val expected: List[CrosswordLine] =
      List(
        CrosswordLine(
          List(
            Open(0,1),
            Open(0,2),
            Open(0,3),
            Open(0,4),
          )
        ),
        CrosswordLine(
          List(
            Open(0,6),
            Open(0,7),
            Open(0,8),
          )
        ),
      )
    println(s"expected: ${expected}")
    println(s"actual  : ${actual}")
    println("---------------------------")
    assertResult(expected)(actual)
  }

  test("crossword toString 0 ") {
    val input = Crossword101Inputs.test0
    val actual = Crosswords101.parseInput(input).toString
    val expected = input.linesIterator.take(10).mkString("\n")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("crossword toString") {
    val input = Crossword101Inputs.test1
    val actual = Crosswords101.parseInput(input).toString
    val expected = input.linesIterator.take(10).mkString("\n")
    println(s"expected: ${expected}")
    assertResult(expected)(actual)
  }

  test("crossword word openings test 0") {
    val input = Crossword101Inputs.test0
    val crossword = Crosswords101.parseInput(input)
    val actual = crossword.remainingWordOpenings.sorted
    val expected = List(
      CrosswordLine(List(Open(9,5),Open(8,5),Open(7,5),Open(6,5),Open(5,5),Open(4,5),Open(3,5))),
      CrosswordLine(List(Open(5,1), Open(4,1), Open(3,1), Open(2,1), Open(1,1), Open(0,1))),
      CrosswordLine(List(Open(3,5), Open(3,4), Open(3,3), Open(3,2), Open(3,1))),
      CrosswordLine(List(Open(7,7), Open(7,6), Open(7,5), Open(7,4), Open(7,3), Open(7,2))),
    ).sorted
    println(s"crossword:\n${crossword}")
    println(s"expected : ${expected}")
    println(s"actual   : ${actual}")
    assertResult(expected)(actual)
  }

  test("crossword openings") {
    val input = Crossword101Inputs.test0MostlyFull
    val crossword = Crosswords101.parseInput(input)
    val actual = crossword.remainingWordOpenings.sorted
    val expected =
      List(
        CrosswordLine(List(
          Open(9,5),
          Open(8,5),
          Letter('A',7,5),
          Open(6,5),
          Open(5,5),
          Open(4,5),
          Letter('I',3,5),
        )),
      ).sorted
    println(s"crossword:\n${crossword}")
    println(s"expected : ${expected}")
    println(s"actual   : ${actual}")
    assertResult(expected)(actual)

  }

  test("test0 full test") {
    val input = Crossword101Inputs.test0
    val crossword = Crosswords101.parseInput(input)
    val actual = Crosswords101.solveCrossword(crossword).toString
    val expected = Crossword101Inputs.result0
    println(s"expected :\n${expected}")
    println(s"actual   :\n${actual}")
    assertResult(expected)(actual)
  }

  test("test1 full test") {
    val input = Crossword101Inputs.test1
    val crossword = Crosswords101.parseInput(input)
    val actual = Crosswords101.solveCrossword(crossword).toString
    val expected = Crossword101Inputs.result1
    println(s"expected :\n${expected}")
    println(s"actual   :\n${actual}")
    assertResult(expected)(actual)
  }

  test("test3 full test") {
    val input = Crossword101Inputs.test3
    val crossword = Crosswords101.parseInput(input)
    val actual = Crosswords101.solveCrossword(crossword).toString
    val expected = Crossword101Inputs.result3
    println(s"expected :\n${expected}")
    println(s"actual   :\n${actual}")
    assertResult(expected)(actual)
  }

}
