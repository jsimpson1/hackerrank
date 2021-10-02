package hackerrank.functionalprogramming.functionalstructures.data

import scala.io.Source

object JohnAndFenceInputs {

  def inputStrToHeights(str: String): Vector[Int] =
    str
      .split("\n")(1)
      .split(" ")
      .map(_.toInt)
      .toVector

  val case1: String = """100
                |1370 4873 2981 478 4760 5191 6872 6665 3327 3106 9828 9991 208 1667 8408 6876 4872 320 1675 747 7706 4165 1579 2988 1126 2093 1313 5300 2111 6948 6838 9833 1821 6171 310 2932 7713 3533 9596 1039 6639 5775 1030 3198 7441 5789 6425 8665 6108 8099 9411 3814 8616 989 6801 9741 9433 4465 5040 1544 1412 8230 7728 3232 4400 4389 2515 8464 7922 8463 9503 912 589 532 461 4382 6320 6885 3046 2427 1335 8808 2592 6302 6149 5744 6043 5581 208 7434 3476 1620 2015 7555 1203 2766 1944 3718 1230 6217""".stripMargin

  val case1Heights: Vector[Int] =
    inputStrToHeights(case1)

  val case2 = """100
                |8792 2599 7827 3650 6439 8772 544 6209 4232 4068 2228 7146 2529 4268 5754 8204 3523 9142 1013 5337 9782 1029 9753 3619 6503 4585 8423 8093 1046 3866 139 9837 2816 7965 9838 5606 3088 382 1814 3672 4449 394 7169 3329 4661 2922 1532 8183 8415 8896 9871 8196 6277 9623 8166 2779 4208 6588 871 1605 6806 7362 7793 5973 1678 7630 1579 4766 4363 3392 4789 5164 137 1957 8492 1149 1230 6376 5683 9644 5271 5554 4191 1547 1528 2356 678 5735 5296 7900 3691 8453 5261 1483 4425 6939 9113 6003 8056 9827""".stripMargin


  val case4: String = {
    val s = Source.fromFile("/Users/flow/code/jeremy/hackerrank/test_cases/johnAndFencesTestcase4.txt")
    s.mkString
  }

}
