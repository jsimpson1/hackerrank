package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.MirkoAtTheConstructionSite
import org.scalatest.funsuite.AnyFunSuite

class MirkoAtTheConstructionSiteTest extends AnyFunSuite {

  def doTest(inputStr: String, expected: List[Int]): Unit = {

    val actual: List[Int] = MirkoAtTheConstructionSite.solve(inputStr)

    assert(actual == expected)
  }

  test("case 0") {

    val inputStr = """3 6
                     |7 5 1
                     |1 2 3
                     |0
                     |1
                     |2
                     |3
                     |4
                     |5""".stripMargin

    val expected = List(1, 1, 2, 2, 3, 3)

    doTest(inputStr, expected)
  }


}
