package hackerrank.recursion

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Crosswords101 {

  import model._

  def main(args: Array[String]): Unit = {
    val input = Crossword101Inputs.test3

    println("input:")
    val crossword = parseInput(input)
    println("output:")
    println(solveCrossword(crossword).toString)
//    printSolvedCrossword(crossword)
  }

  object model {
    case class Coordinate(rowIndex: Int, columnIndex: Int) extends Ordered[Coordinate] {
      override def toString: String = s"($rowIndex,$columnIndex)"

      override def compare(that: Coordinate): Int = {
        val xCompare = rowIndex.compare(that.rowIndex)
        if (xCompare == 0) {
          columnIndex.compare(that.columnIndex)
        } else {
          xCompare
        }
      }
    }

    object CrosswordSquare {
      def parse(c: Char, coordinate: Coordinate): CrosswordSquare = c match {
        case Open.value => Open(coordinate)
        case Closed.value => Closed(coordinate)
        case letter => Letter(letter, coordinate)
      }
    }

    sealed trait CrosswordSquare extends Ordered[CrosswordSquare]{
      def value: Char
      def coordinate: Coordinate
      def isClosed: Boolean = value.equals(Closed.value)
      def isOpen: Boolean = value.equals(Open.value)
      def isLetter: Boolean = !isOpen && !isClosed
      def hasSameCoordinates(cs: CrosswordSquare): Boolean = coordinate.equals(cs.coordinate)

      override def compare(that: CrosswordSquare): Int =
        coordinate.compare(that.coordinate)
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

    case class Word(value: String) extends Ordered[Word]{
      def length: Int = value.length

      override def compare(that: Word): Int =
        value.compareTo(that.value)
    }

    case class CrosswordLine(squares: List[CrosswordSquare]) extends Ordered[CrosswordLine]{

      lazy val columnIndexToSquares: Map[Int, List[CrosswordSquare]] =
        squares
          .groupBy(_.coordinate.columnIndex)

      lazy val rowIndexToSquares: Map[Int, List[CrosswordSquare]] =
        squares
          .groupBy(_.coordinate.rowIndex)

      lazy val allWordOpenings: List[CrosswordLine] = {
        @tailrec
        def resolvedOpenLines(
          squares: List[CrosswordSquare],
          sequentialSquare: List[CrosswordSquare],
          result: List[CrosswordLine]
        ): List[CrosswordLine] = {
          squares
            .headOption match {
            case None =>
              if ( sequentialSquare.nonEmpty ){
                result :+ CrosswordLine(sequentialSquare)
              } else {
                result
              }

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

      override def compare(that: CrosswordLine): Int = {
        squares
          .zip(that.squares)
          .map{ squarePair =>
            squarePair._1.compare(squarePair._2)
          }
          .sum
      }

      def sortedSquares: List[CrosswordSquare] =
        if (isRow) squares.sortBy(_.coordinate.rowIndex).reverse
        else squares.sortBy(_.coordinate.columnIndex).reverse

      def canMatch(word: Word): Boolean = {
        if ( word.length != squares.length) {
          false
        } else {
          val ss =
          sortedSquares
            .zip(word.value)
          ss
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

      def isPartiallyFilled: Boolean =
        squares
          .exists(_.isLetter) && !isFilled

      def isRow: Boolean =
        rowIndexToSquares
          .size == 1

      def isColumn: Boolean =
        columnIndexToSquares
          .size == 1

      def rowIndex: Option[Int] =
        if ( isRow )
          squares
            .headOption
            .map(_.coordinate.rowIndex)
        else
          None

      def columnIndex: Option[Int] =
        if ( isRow )
          squares
            .headOption
            .map(_.coordinate.columnIndex)
        else
          None

      def length: Int = squares.length

    }

    case class PlaceWordResult(remainingWord: Option[Word], crossword: Crossword)

    case class Crossword(originalWords: List[Word], squares: List[CrosswordSquare]) {

      def placeWords(words: List[Word]): Crossword = {
        def r(remainingWords: List[Word]): Crossword = {
          remainingWords match {
           case Nil =>
             this
           case word :: remainder =>

             placeWord(word)match {
               case PlaceWordResult(unplacedWOrd, crossword) =>
                 if ( crossword.isCrosswordComplete ) {
                   crossword
                 } else {
                   println(s"placeWords -- next unplacedWOrd=${unplacedWOrd}, crossword:\n${crossword}")
                   unplacedWOrd match {
                     case None =>
                       crossword.placeWords(remainder)
                     case Some(w) =>
                       if ( remainder.isEmpty ){
                         crossword
                       } else {
                         val nextRemainder = remainder :+ w
                         println(s"placeWords -- next unplacedWOrd=${unplacedWOrd}, crossword:\n${crossword}")
                         crossword.placeWords(nextRemainder)
                       }
                   }
                 }
             }
         }
        }
        r(words)
      }

      def placeWord(word: Word): PlaceWordResult = {
        val potentialOpenings =
        remainingWordOpenings
          .filter { line =>
            val filled = !line.isFilled
            val canMatch = line.canMatch(word)
            filled && canMatch
          }
        potentialOpenings match {
            case List(line) =>
              PlaceWordResult(None, updateLine(line, word))
            case multiLines =>
              multiLines
                .find(line =>
                  line.isPartiallyFilled
                ).map(line =>
                  PlaceWordResult(None, updateLine(line, word))
                ).getOrElse(
                  PlaceWordResult(Some(word), this)
                )
          }
      }

      def updateSquare(square: CrosswordSquare, char: Char): Crossword = {
        val updatedSquares = squares.map(s =>
            if (s.hasSameCoordinates(square))
              Letter(char, square.coordinate)
            else
              s
          )
        this.copy(squares = updatedSquares)
      }

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

        r(squareToChar, this)
      }

      def resolveLines(isRow: Boolean): List[CrosswordLine] = {
        squares
          .groupBy { square =>
            val coord = square.coordinate
            if (isRow)
              coord.rowIndex
            else
              coord.columnIndex
          }.map(v =>
            CrosswordLine(v._2)
          )
          .toList
      }

      val rows: List[CrosswordLine] = resolveLines(true)

      val columns: List[CrosswordLine] = resolveLines(false)

      val rowWordOpenings: List[CrosswordLine] =
        rows
          .flatMap(_.allWordOpenings)
          .filterNot(_.isFilled)

      val columnWordOpenings: List[CrosswordLine] = {
       val openings =
         columns
          .flatMap(_.allWordOpenings)

        openings.filterNot(_.isFilled)
      }

      lazy val remainingWordOpenings: List[CrosswordLine] =
        (rowWordOpenings ++ columnWordOpenings)
          .filter(cl => originalWords.map(_.length).contains(cl.length))

      def isCrosswordComplete: Boolean = remainingWordOpenings.isEmpty

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

      def display: Unit =
      println(
        s"""originalWords:
           |${originalWords.map(w => s"${w.value.length} - ${w.value}").mkString("\n")}
           |crossword:
           |${toString}""".stripMargin
      )

    }

    case class Input(words: List[Word], crossword: Crossword)

  }

  def parseInput(input: String): Crossword = {
    val lines = input.lines.toList
    try {
      val words = lines(10).split(";").toList.map(Word)
      Crossword(
        words,
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
    } catch {
      case e: Exception =>
        println(s"parseInput -- error parsing lines=${lines}\n${e.getMessage}")
        throw e
    }
  }

  def printSolvedCrossword(c: Crossword): Unit =
    println(solveCrossword(c))

  def sortWordsByLength(words: List[Word]): List[Word] = {
    val wordLengthToWords: List[(Int, List[Word])] =
      words
        .groupBy(_.length)
        .toList
        .sortBy(v => v._1)

    val wordsOfDistinctSize: List[Word] =
      wordLengthToWords
        .filter(_._2.size == 1)
        .flatMap(_._2)
        .sorted

    val wordsOfNonDistinctSize: List[Word] =
      wordLengthToWords
        .filter(_._2.size != 1)
        .flatMap(_._2)
        .sorted

    wordsOfDistinctSize ++ wordsOfNonDistinctSize
  }

  def solveCrossword(crossword: Crossword): Crossword = {
    crossword
      .originalWords match {
        case Nil =>
          crossword
        case words =>
          val sortedWords = sortWordsByLength(words)
          crossword.placeWords(sortedWords)
      }
  }


}
