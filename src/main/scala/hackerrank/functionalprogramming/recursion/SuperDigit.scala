package hackerrank.functionalprogramming.recursion

object SuperDigit {

  def main(args: Array[String]) {
    val input = "3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736 100000"
    val nAndk = input.split(" ")
    //    println(superDigit(nAndk(0), nAndk(1).toInt))

    val nk = input.split(" ").map(_.map(_.asDigit).sum).map(_ % 9).product
    val magic = if (nk % 9 == 0) 9 else nk % 9
    println(magic)
  }

  def superDigit(input: Array[String]): Int = {
    input
      .map(
        _.map(
          _.asDigit
        ).sum
      )
      .map(_ % 9)
      .product % 9 match {
      case 0 => 9
      case v => v
    }
  }

  //  def superDigit(n: String, k: String): Int = {
  //    @tailrec
  //    def r(str: String): Int = {
  //      str
  //        .split("") match {
  //        case Array(h) =>
  //          h.toInt
  //        case values =>
  //          r(values.map(_.toInt).sum.toString)
  //      }
  //    }
  //    r(n * k.toInt)
  //  }

}
