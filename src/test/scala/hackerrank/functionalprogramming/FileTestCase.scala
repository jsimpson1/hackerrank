package hackerrank.functionalprogramming

import scala.io.Source

object FileTestCase {

  lazy val testCasesRoot = "/Users/flow/code/jeremy/hackerrank/test_cases"

  sealed trait HasTestFiles {

    def testCaseFolder: String

    def fileContents(fileName: String): String = {
      val source = Source.fromFile(s"${testCaseFolder}/${fileName}")
      source.mkString
    }

  }

  case object FunctionalStructureTestFiles extends HasTestFiles {
    override def testCaseFolder: String = s"${testCasesRoot}/functionalstructure"
  }

  case object MemoizationAndDpTestFiles extends HasTestFiles {
    override def testCaseFolder: String = s"${testCasesRoot}/memoizationanddp"
  }

  case object RecursionTestFiles extends HasTestFiles {
    override def testCaseFolder: String = s"${testCasesRoot}/recursion"
  }


}

