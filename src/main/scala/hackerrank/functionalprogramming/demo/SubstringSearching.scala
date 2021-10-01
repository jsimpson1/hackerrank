package hackerrank.functionalprogramming.demo

object SubstringSearching {

  import scala.io.StdIn._
  import scala.annotation.tailrec

  def main(args: Array[String]): Unit = {

    val inputStr = """4
                     |abcdef
                     |def
                     |computer
                     |muter
                     |stringmatchingmat
                     |ingmat
                     |videobox
                     |videobox""".stripMargin

    val lines = inputStr.split("\n")

    val numOfCases = lines.head.toInt

    (1 to numOfCases).foreach { i =>
      val text = lines(i)
      val pat = lines(i + 1)
      printMsg(text, pat)
    }
  }



  def printMsg(text: String, pattern: String): Unit = {
    if ( hasPattern(text, pattern) ) {
      println("YES")
    } else {
      println("NO")
    }
  }

  def hasPattern(text: String, pattern: String): Boolean = {
    lazy val fullPattern = pattern.toList
    @tailrec
    def r(chars: List[Char], pat: List[Char], charsSoFar: List[Char]): Boolean = {
      chars match {
        case Nil =>
          if ( pat.isEmpty )
            true
          else
            false
        case ch :: cTail =>
          pat match {
            case Nil =>
              true
            case ph :: pTail =>
              if ( ph == ch ) {
                r(cTail, pTail, ch :: charsSoFar)
              } else {
                if ( List(ch) == charsSoFar ) {
                  r(cTail, fullPattern.drop(1), Nil)
                } else {
                  r(cTail, fullPattern, Nil)
                }
              }
          }
      }
    }
    r(text.toList, fullPattern, Nil)
  }


}
