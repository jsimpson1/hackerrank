package hackerrank.functionalprogramming.functionalstructure
import scala.collection.immutable

object JohnAndFencesDemo extends App {

//  val str = """1370 4873 2981 478 4760 5191 6872 6665 3327 3106 9828 9991 208 1667 8408 6876 4872 320 1675 747 7706 4165 1579 2988 1126 2093 1313 5300 2111 6948 6838 9833 1821 6171 310 2932 7713 3533 9596 1039 6639 5775 1030 3198 7441 5789 6425 8665 6108 8099 9411 3814 8616 989 6801 9741 9433 4465 5040 1544 1412 8230 7728 3232 4400 4389 2515 8464 7922 8463 9503 912 589 532 461 4382 6320 6885 3046 2427 1335 8808 2592 6302 6149 5744 6043 5581 208 7434 3476 1620 2015 7555 1203 2766 1944 3718 1230 6217"""
//
//  //  val str = {
//  //    val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/johnAndFencesTestcase4.txt")
//  //    s.mkString.split("\n")(1)
//  //  }
//
//  printWithIndex(str)
//
//  def printWithIndex(str: String): Unit =
//    println(
//      str.split(" ").toList.map(_.toInt).zipWithIndex.map(v => (v._2, v._1)).mkString("\n")
//    )
//
//
//  println("\n\nfor charting\n")
//  println(
//    str.split(" ").toList.map(_.toInt).zipWithIndex.map(v => (v._2 * 500, v._1)).mkString("\n")
//  )

  val case2Solution: Double = 50216

  val case2PossibleValues: IndexedSeq[(Int, Double)] =
    (1 to 100)
      .map(v => (v, case2Solution / v))
      .filter(v => v._2.isValidInt)

  println(case2PossibleValues.mkString("\n"))


}
