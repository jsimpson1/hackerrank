package hackerrank.functionalprogramming.functionalstructure

import hackerrank.functionalprogramming.FileTestCase.FunctionalStructureTestFiles
import hackerrank.functionalprogramming.functionalstructures.TreeManager
import hackerrank.functionalprogramming.functionalstructures.TreeManager.model.Tree
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import scala.collection.immutable.HashMap
import scala.io.Source

class TreeManagerTest extends AnyFunSuite {

  val case0InputStr = """11
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

  case class ManagerAndLoggingState(manager: TreeManager, loggingOutPut: List[Int])

  def streamToNodeValues(stream: ByteArrayOutputStream): List[Int] = {
    val str = stream.toString
    str match {
      case "" => Nil
      case s =>
        s.split("\n").toList.map(_.toInt)
    }

  }

  def testDoOperationsLogging(inputStr: String): ManagerAndLoggingState = {
    val stream = new ByteArrayOutputStream()
    val manager: TreeManager =
      Console
        .withOut(stream) {
          TreeManager.doOperations(inputStr)
        }
    val output = streamToNodeValues(stream)
    ManagerAndLoggingState(manager, output)
  }

  def onlyFirstXOperations(inputStr: String, numOfOperations: Int): String = {
    inputStr
      .split("\n")
      .tail
      .take(numOfOperations)
      .mkString("\n")
  }

  def runDoOperationsTest(caseNum: Int, numOfOperations: Int, expected: ManagerAndLoggingState): Unit = {
    val testName: String = s"runDoOperationsTest -- case $caseNum - operation $numOfOperations"
    test(testName) {
      val inputStr = onlyFirstXOperations(case0InputStr, numOfOperations)

      val actual: ManagerAndLoggingState = testDoOperationsLogging(inputStr)

      assert(actual == expected)
    }
  }

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 1,
    expected = ManagerAndLoggingState(
      TreeManager(
        0,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
        )
      ),
      Nil
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 2,
    expected = ManagerAndLoggingState(
      TreeManager(
        0,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
        )
      ),
      List(1)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 3,
    expected = ManagerAndLoggingState(
      TreeManager(
        0,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(Nil, 2, Nil)),
        )
      ),
      List(1)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 4,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(Nil, 2, Nil)),
        )
      ),
      List(1)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 5,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(Nil, 2, List(3))),
        )
      ),
      List(1)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 6,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(List(2), 3, Nil)),
        )
      ),
      List(1)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 7,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(List(2), 3, Nil)),
        )
      ),
      List(1,3)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 8,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(List(2), 3, List(4))),
        )
      ),
      List(1,3)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 9,
    expected = ManagerAndLoggingState(
      TreeManager(
        0,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(Nil, 2, List(4))),
        )
      ),
      List(1,3)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 10,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(List(2), 4, Nil)),
        )
      ),
      List(1,3)
    )
  )

  runDoOperationsTest(
    caseNum = 0,
    numOfOperations = 11,
    expected = ManagerAndLoggingState(
      TreeManager(
        1,
        HashMap[Int, Tree](
          (0, Tree(Nil, 1, Nil)),
          (1, Tree(List(2), 4, Nil)),
        )
      ),
      List(1,3,4)
    )
  )

  test("tree focus") {
    val treeWithOnlyRight = Tree(Nil, 1, List(2))
    assert(
      treeWithOnlyRight.changeFocus(1) == Tree(Nil, 1, List(2))
    )
    assert(
      treeWithOnlyRight.changeFocus(2) == Tree(List(1), 2, Nil)
    )
    val treeWithOnlyLeft = Tree(List(1), 2, Nil)
    assert(
      treeWithOnlyLeft.changeFocus(1) == Tree(Nil, 1, List(2))
    )
    assert(
      treeWithOnlyLeft.changeFocus(2) == Tree(List(1), 2, Nil)
    )
    val treeWithMoreThanThree = Tree(List(1,2), 3, List(4))
    assert(
      treeWithMoreThanThree.changeFocus(1) == Tree(Nil, 1, List(2, 3, 4))
    )
    assert(
      treeWithMoreThanThree.changeFocus(2) == Tree(List(1), 2, List(3, 4))
    )
    assert(
      treeWithMoreThanThree.changeFocus(3) == Tree(List(1,2), 3, List(4))
    )
    assert(
      treeWithMoreThanThree.changeFocus(4) == Tree(List(1,2,3), 4, Nil)
    )
  }

  test("tree visitLeft") {
    assert(
      Tree(List(1), 2, Nil).visitLeft == Tree(Nil, 1, List(2))
    )
    assert(
      Tree(List(1, 2), 3, Nil).visitLeft == Tree(List(1), 2, List(3))
    )
    assert(
      Tree(List(1, 2), 3, List(4)).visitLeft == Tree(List(1), 2, List(3,4))
    )
  }

  test("treeManager visitParent") {

    val map = HashMap[Int, Tree](
      (0, Tree(Nil, 1, Nil)),
      (1, Tree(List(2), 4, Nil)),
    )

    val actual = TreeManager(1, map).visitParent

    val expected = TreeManager(0, map)

    assert(actual == expected)
  }

  test("tree insertLeft") {
    assert(
      Tree(Nil, 1, Nil).insertLeft(2) == Tree(List(2), 1, Nil)
    )
    assert(
      Tree(List(2), 1, Nil).insertLeft(3) == Tree(List(2, 3), 1, Nil)
    )
  }

  val testFiles = FunctionalStructureTestFiles

  test("case 8") {

    val inputStr = testFiles.fileContents("treeManagerCase8.txt")

    val actual: List[Int] = testDoOperationsLogging(inputStr).loggingOutPut

    val expected: List[Int] =
      testFiles
        .fileContents("treeManagerCase8Result.txt").split("\n")
        .map(_.toInt)
        .toList

    assert(actual == expected)
  }

}
