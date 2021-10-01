package hackerrank.functionalprogramming

object StringCompressionDemo extends App {

  val input = "aaabaaaaccaaaade"

  println(compressString(input))

  case class RepeatChar(c: Option[Char], count: Int) {
    override def toString: String =
      c
        .map(cc => if (count > 1) s"$cc$count" else cc.toString)
        .getOrElse("")
  }

  def compressString(s: String): String = {
    def r(str: String, rChar: RepeatChar, result: String): String = {
      str.headOption match {
        case None =>
          result + rChar.toString
        case Some(c) =>
          rChar.c match {
            case None =>
              r(str.tail, RepeatChar(Some(c), 1), result)
            case Some(rc) =>
              if (rc == c) {
                r(str.tail, rChar.copy(count = rChar.count + 1), result)
              } else {
                r(str.tail, RepeatChar(Some(c), 1), s"${result}${rChar.toString}")
              }
          }
      }
    }
    r(s, RepeatChar(None, 0), "")
  }



}
