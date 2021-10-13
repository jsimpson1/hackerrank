package hackerrank.functionalprogramming.functionalstructures.model


import scala.annotation.tailrec
import scala.math.sqrt
import scala.reflect.ClassTag

case class Indexes(individualCalc: List[Int], block: List[Int])

trait SqrtDecompositionValue[T] {

  def apply(i: Int): T

  def makeBlock(l: Array[Int]): T

  def aggregate(a: T, b: T): T

  def aggregate(a: Int, b: T): T =
    aggregate(apply(a), b)

}

case class SqrtDecomposition[T:ClassTag](
  values: Array[Int],
)(
  implicit
    sdValue: SqrtDecompositionValue[T]
) {

  val maxIndex: Int = values.length - 1

  val blockSize: Int = sqrt(values.length).toInt

  val valueBlocks: Array[T] =
    values
      .sliding(blockSize, blockSize)
      .map(v=>
        sdValue.makeBlock(v)
      ).toArray


  def partialBlockCalc(indexes: List[Int]): Option[T] = {
    @tailrec
    def r(indexes: List[Int], result: T): T = {
      indexes match {
        case Nil =>
          result
        case h :: tail =>
          r(
            tail,
            sdValue.aggregate(values(h), result)
          )
      }
    }

    values match {
      case Array() =>
        None
      case values =>
        indexes match {
          case Nil =>
            None
          case indexes =>
            Some(
              r(
                indexes,
                sdValue.apply(values(indexes.head))
              )
            )
        }

    }
  }

  def fullBlocksCalc(indexes: List[Int]): Option[T] = {

    @tailrec
    def r(indexes: List[Int], result: T): T = {
      indexes match {
        case Nil =>
          result
        case h :: tail =>
          r(
            tail,
            sdValue.aggregate(result, valueBlocks(h))
          )
      }
    }

    valueBlocks match {
      case Array() =>
        None
      case values =>
        indexes match {
          case Nil =>
            None
          case indexes =>
            Some(
              r(indexes, values(indexes.head))
            )
        }
    }

  }

  def queryIndexes(startIndex: Int, endIndex: Int): Option[Indexes] = {

    if ( startIndex < 0 || endIndex > maxIndex || endIndex < startIndex) {
      None
    } else {
      val rawIndexes = (startIndex to endIndex).toList
      val firstBlockStart = rawIndexes.indexWhere(_%blockSize == 0)
      firstBlockStart match {
        case -1 =>
          Some(Indexes(rawIndexes, Nil))
        case blockStart =>
          val (firstPartialBlock: List[Int], rest: List[Int]) = rawIndexes.splitAt(blockStart)

          val (fullBlocks, lastPartialBlock) =
            rest
              .sliding(blockSize, blockSize)
              .toList match {
              case remainingBlocks =>
                remainingBlocks match {
                  case blocks :+ lastBlock =>
                    if ( lastBlock.size == blockSize ) {
                      (remainingBlocks, Nil)
                    } else {
                      (blocks, lastBlock)
                    }
                }

            }

          lazy val blockIndexAdjustment: Int =
            rest.head/blockSize

          val blockIndexes: List[Int] =
            fullBlocks
              .indices
              .map(index =>
                index + blockIndexAdjustment
              ).toList

          Some(
            Indexes(
              firstPartialBlock ++ lastPartialBlock,
              blockIndexes
            )
          )
      }
    }


  }

  def query(startIndex: Int, endIndex: Int): Option[T] = {

    if ( startIndex < 0 || endIndex > maxIndex || endIndex < startIndex) {
      None
    } else {
      queryIndexes(startIndex, endIndex) match {
        case None =>
          None
        case Some(indexes) =>
          indexes match {
            case Indexes(Nil, Nil) =>
              None
            case Indexes(pricesIndexes, Nil) =>
              partialBlockCalc(pricesIndexes)
            case Indexes(Nil, blockIndexes) =>
              fullBlocksCalc(blockIndexes)
            case Indexes(partialBlockIndexes, fullBlockIndexes) =>
              partialBlockCalc(partialBlockIndexes)
                .flatMap { partial =>
                  fullBlocksCalc(fullBlockIndexes)
                    .map { full =>
                      sdValue.aggregate(partial, full)
//                        partial.combine(full)
                    }
                }
          }
      }
    }

  }

}
