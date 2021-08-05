package hackerrank.recursion

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.ListMap

object Crosswords101 {

  import model._

  def main(args: Array[String]): Unit = {
    val input = Crossword101Inputs.test0

    println("input:")
    val i = parseInput(input)
    printInput(i)
    println("output:")
    printSolvedCrossword(i)
  }

  object model {
    case class Coordinate(rowIndex: Int, columnIndex: Int) {
      override def toString: String = s"($rowIndex,$columnIndex)"
      //      def isNextTo(c: Coordinate): Boolean = {
      //        List(
      //          c.columnIndex -1,
      //          c.columnIndex + 1
      //        ).contains(columnIndex) ||
      //          List(
      //            c.rowIndex -1,
      //            c.rowIndex + 1
      //          ).contains(rowIndex)
      //      }
    }

    object CrosswordSquare {
      def parse(c: Char, coordinate: Coordinate): CrosswordSquare = c match {
        case Open.value => Open(coordinate)
        case Closed.value => Closed(coordinate)
        case letter => throw new RuntimeException(s"no letters expected but parse hit ${letter}")
      }
    }

    sealed trait CrosswordSquare {
      def value: Char

      def coordinate: Coordinate

      def isClosed: Boolean = value.equals(Closed.value)

      def isOpen: Boolean = value.equals(Open.value)

      def isLetter: Boolean = !isOpen && !isClosed

      def hasSameCoordinates(cs: CrosswordSquare): Boolean = coordinate.equals(cs.coordinate)
      //      def touches(cs: CrosswordSquare): Boolean = this.coordinate.isNextTo(cs.coordinate)
    }

    object Open {
      val value: Char = '-'

      def apply(rowIndex: Int, columnIndex: Int): Open =
        Open(Coordinate(rowIndex, columnIndex))
    }

    object Closed {
      val value: Char = '+'

      def apply(rowIndex: Int, columnIndex: Int): Closed =
        Closed(Coordinate(rowIndex, columnIndex))
    }

    case class Open(coordinate: Coordinate) extends CrosswordSquare {
      val value: Char = Open.value
    }

    case class Closed(coordinate: Coordinate) extends CrosswordSquare {
      val value: Char = Closed.value
    }

    object Letter {
      def apply(value: Char, rowIndex: Int, columnIndex: Int): Letter =
        Letter(value, Coordinate(rowIndex, columnIndex))
    }

    case class Letter(value: Char, coordinate: Coordinate) extends CrosswordSquare

    case class Word(value: String) {
      def length: Int = value.length
    }

    case class CrosswordLine(squares: List[CrosswordSquare]) {
      def sortedSquares: List[CrosswordSquare] =
        if (isRow) squares.sortBy(_.coordinate.rowIndex).reverse
        else squares.sortBy(_.coordinate.columnIndex).reverse

      def canMatch(word: Word): Boolean = {
        if ( word.length != squares.length) {
          false
        } else {
          sortedSquares
            .zip(word.value)
            .forall{ squareToChar =>
              squareToChar._1 match {
                case Letter(c, _) =>
                  c == squareToChar._2
                case Open(_) =>
                  true
                case Closed(_) =>
                  false
              }
            }
        }
      }

      def isFilled: Boolean =
        squares
          .forall(_.isLetter)

      def isRow: Boolean =
        squares
          .groupBy(_.coordinate.rowIndex)
          .size == 1

      def isColumn: Boolean =
        squares
          .groupBy(_.coordinate.columnIndex)
          .size == 1

      def size: Int = squares.size

      def getAllWordOpenings: List[CrosswordLine] = {
        @tailrec
        def resolvedOpenLines(
          squares: List[CrosswordSquare],
          sequentialSquare: List[CrosswordSquare],
          result: List[CrosswordLine]
        ): List[CrosswordLine] = {
          squares
            .headOption match {
            case None =>
              result
            case Some(nextSquare) =>
              if (!nextSquare.isClosed) {
                val nextSequentialSquare = nextSquare :: sequentialSquare
                resolvedOpenLines(squares.tail, nextSequentialSquare, result)
              } else {
                if (sequentialSquare.isEmpty) {
                  resolvedOpenLines(squares.tail, Nil, result)
                } else {
                  val opening = CrosswordLine(sequentialSquare)
                  resolvedOpenLines(squares.tail, Nil, result.::(opening))
                }
              }
          }
        }

        resolvedOpenLines(squares, Nil, Nil)
      }
    }

    case class PlaceWordResult(success: Boolean, crossword: Crossword)

    case class Crossword(squares: List[CrosswordSquare]) {

      def placeWord(word: Word): PlaceWordResult = {
        getRemainingWordOpenings
          .filter(line =>
            !line.isFilled && line.canMatch(word)
        ) match {
            case List(line) =>
              PlaceWordResult(success = true, updateLine(line, word))
            case _ =>
              PlaceWordResult(success = false, this)
          }
      }

      def updateSquare(square: CrosswordSquare, char: Char): Crossword =
        Crossword(
          squares
            .map(s =>
              if (s.hasSameCoordinates(square))
                Letter(char, square.coordinate)
              else
                s
            )
        )

      def updateLine(line: CrosswordLine, word: Word): Crossword = {
        val squareToChar: List[(CrosswordSquare, Char)] =
          line
            .sortedSquares
            .zip(word.value)

        @tailrec
        def r(values: List[(CrosswordSquare, Char)], result: Crossword): Crossword = {
          values
            .headOption match {
            case None =>
              result
            case Some((cs, c)) =>
              r(values.tail, result.updateSquare(cs, c))
          }
        }

        r(squareToChar, Crossword(squares))
      }

      def rows: List[CrosswordLine] =
        squares
          .groupBy(square =>
            square
              .coordinate
              .rowIndex
          ).map(v =>
          CrosswordLine(v._2)
        ).toList

      def columns: List[CrosswordLine] =
        squares
          .groupBy(square =>
            square
              .coordinate
              .columnIndex
          ).map(v =>
          CrosswordLine(v._2)
        ).toList

      def getRowWordOpenings: List[CrosswordLine] =
        rows
          .flatMap(_.getAllWordOpenings)

      def getColumnWordOpenings: List[CrosswordLine] =
        columns
          .flatMap(_.getAllWordOpenings)

      def getRemainingWordOpenings: List[CrosswordLine] = getRowWordOpenings ++ getColumnWordOpenings

      override def toString: String = {
        ListMap(
          squares
            .groupBy(_.coordinate.rowIndex)
            .toSeq
            .sortBy(_._1): _*)
          .map(v =>
            v._2
              .map(_.value)
              .mkString("")
          )
          .mkString("\n")
      }

    }

    case class Input(words: List[Word], crossword: Crossword)

  }

  def printInput(i: Input): Unit =
    println(
      s"""words:
         |${i.words.map(w => s"${w.value.length} - ${w.value}").mkString("\n")}
         |crossword:
         |${i.crossword}""".stripMargin
    )

  def parseInput(input: String): Input = {
    val lines = input.lines.toList
    val c = Crossword(
      List
        .range(0, 10)
        .flatMap { rowIndex =>
          lines(rowIndex)
            .zipWithIndex
            .map { c =>
              CrosswordSquare
                .parse(c._1, Coordinate(rowIndex, c._2))
            }
            .toList
        }
    )
    val words = lines(10).split(";").toList.map(Word)
    Input(words, c)
  }

  def printSolvedCrossword(input: Input): Unit =
    println(solveCrossword(input.words, input.crossword))

  case class CrosswordIteration(remainingWords: List[Word], currentCrossword: Crossword)

  def resolvedWordsByLength(words: List[Word], crossword: Crossword): CrosswordIteration = {
    val wordLengthToWords: Map[Int, List[Word]] =
      words
        .groupBy(_.length)

    val wordsOfDistinctLength: Map[Int, List[Word]] =
      wordLengthToWords
        .filter(_._2.size == 1)

    val remainingWords: List[Word] =
      wordLengthToWords
        .filter(_._2.size > 1)
        .values
        .flatten
        .toList

    def r(values: Map[Int, List[Word]], result: Crossword): Crossword = {
      val lineToWords =
      values
        .flatMap { lengthToWord =>
          crossword
            .getRemainingWordOpenings
            .filter(line =>
              line.size == lengthToWord._1
            ) match {
            case List(line) =>
              Some((line, lengthToWord._2))
            case _ =>
              None
          }
        }

    lineToWords
      .headOption match {
      case None =>
        CrosswordIteration(words, crossword)
      case Some(lineToWord) =>
        lineToWord match {
          case (line, List(word)) =>
            val remainingWords = words.filterNot(_ == word)
            resolvedWordsByLength(remainingWords, crossword.updateLine(line, word))
          case (_, wordsOfSameLength) =>
            val remainingWords = words.filterNot(w => wordsOfSameLength.contains(w))
            resolvedWordsByLength(remainingWords, crossword)
        }
    }
  }

    val nextCrossword = r(wordsOfDistinctLength, crossword)

    CrosswordIteration(remainingWords, nextCrossword)
  }

  @tailrec
  def placeWords(currentIteration: CrosswordIteration): Crossword = {
    currentIteration
      .remainingWords match {
        case Nil =>
          currentIteration
            .currentCrossword
        case word :: remainder =>
          currentIteration
            .currentCrossword
            .placeWord(word)match {
            case PlaceWordResult(true, crossword) =>
              placeWords(CrosswordIteration(remainder, crossword))
            case PlaceWordResult(false, crossword) =>
              placeWords(CrosswordIteration(remainder :+ word, crossword))
          }

      }
  }

  def solveCrossword(words: List[Word], crossword: Crossword): Crossword = {
    if ( words.isEmpty) {
      crossword
    } else {
      val crosswordSeededByDistinctLengths = resolvedWordsByLength(words, crossword)
      println(s"solveCrossword -- crosswordSeededByDistinctLengths=\n${crosswordSeededByDistinctLengths.currentCrossword}")
      placeWords(crosswordSeededByDistinctLengths)
    }
  }


}
