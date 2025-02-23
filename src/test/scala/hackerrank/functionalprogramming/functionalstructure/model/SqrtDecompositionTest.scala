package hackerrank.functionalprogramming.functionalstructure.model

import hackerrank.functionalprogramming.functionalstructures.model.{Indexes, SqrtDecomposition, SqrtDecompositionValue}
import org.scalatest.funsuite.AnyFunSuite

class SqrtDecompositionTest extends AnyFunSuite {

  case class MinMax(min: Int, max: Int) {

    def isInside(value: Int): Boolean =
      value >= min && value <= max

    def isInside(limit: MinMax): Boolean =
      min >= limit.min && max <= limit.max

    def combine(other: MinMax): MinMax =
      MinMax(
        scala.math.min(min, other.min),
        scala.math.max(max, other.max)
      )

  }

  implicit val minMaxValue: SqrtDecompositionValue[MinMax] = new SqrtDecompositionValue[MinMax] {

    override def apply(i: Int): MinMax =
      MinMax(i, i)

    override def makeBlock(l: Array[Int]): MinMax =
      MinMax(l.min, l.max)

    override def aggregate(a: MinMax, b: MinMax): MinMax =
      MinMax(
        scala.math.min(a.min, b.min),
        scala.math.max(a.max, b.max)
      )
  }

  def sqrtDecompositionTest[I,E](input: I, expected: E)(implicit fn: I => E): Unit = {
    assert((input, fn(input)) == (input, expected))
  }


  test("SqrtDecomposition -- partialBlockCalc") {

    val data = SqrtDecomposition(Array(3,5,2,6,1))

    val actual: Option[MinMax] = data.partialBlockCalc((0 to 2).toList)

    val expected =  Some(MinMax(2,5))

    assert(actual == expected)

    val actualFull = data.partialBlockCalc(data.values.indices.toList)

    val expectedFull =  Some(MinMax(1,6))

    assert(actualFull == expectedFull)
  }


  test("SqrtDecomposition -- queryIndexes") {

    val data = SqrtDecomposition(Array(3,5,2,6,0,7,1,8,10,2))

    implicit def fn: ((Int, Int)) => Option[Indexes] =
      a => data.queryIndexes(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((-1, 9), None)
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 10), None)
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((10, 0), None)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 8), Some(Indexes(Nil, List(0,1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((1, 8), Some(Indexes(List(1,2), List(1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((2, 8), Some(Indexes(List(2), List(1,2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((3, 8), Some(Indexes(Nil, List(1,2))))

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((4, 8), Some(Indexes(List(4,5), List(2))))
    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((5, 8), Some(Indexes(List(5), List(2))))


    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((0, 9), Some(Indexes(List(9), List(0,1,2))))
  }

  test("SqrtDecomposition -- queryIndexes  case 4") {

    val data = SqrtDecomposition(
      "1 2 3 2 1".split(" ").map(_.toInt)
    )

    implicit def fn: ((Int, Int)) => Option[Indexes] =
      a => data.queryIndexes(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[Indexes]]((2, 3), Some(Indexes(List(), List(1))))
  }

  test("SqrtDecomposition -- query") {

    val data = SqrtDecomposition(Array(3,5,2,6,0,7,1,8,10,2))

    implicit def fn: ((Int, Int)) => Option[MinMax] =
      a => data.query(a._1, a._2)

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((-1, 9), None)
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 10), None)
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((10, 0), None)

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 9), Some(MinMax(0, 10)))
    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((0, 8), Some(MinMax(0, 10)))

    sqrtDecompositionTest[(Int, Int), Option[MinMax]]((2, 8), Some(MinMax(0, 10)))

  }

}
