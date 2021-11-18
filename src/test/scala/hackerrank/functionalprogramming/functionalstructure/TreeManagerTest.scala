package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.functionalstructures.TreeManager
import org.scalatest.funsuite.AnyFunSuite

class TreeManagerTest extends AnyFunSuite {

  test("case 0") {

    val inputStr = """11
                     |change 1
                     |print
                     |insert child 2
                     |visit child 1
                     |insert right 3
                     |visit right
                     |print
                     |insert right 4
                     |delete
                     |visit child 2
                     |print""".stripMargin

    val stream = new java.io.ByteArrayOutputStream()
    Console
      .withOut(stream) {
        TreeManager.solve(inputStr)
      }

    val expected: List[String] = List(1,3,4).map(_.toString)

    val actual: List[String] = stream.toString.split("\n").toList

    assert(actual == expected)
  }




}
