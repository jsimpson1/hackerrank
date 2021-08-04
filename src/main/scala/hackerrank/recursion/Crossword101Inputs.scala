package hackerrank.recursion

object Crossword101Inputs {

  val test0 = """+-++++++++
                |+-++++++++
                |+-++++++++
                |+-----++++
                |+-+++-++++
                |+-+++-++++
                |+++++-++++
                |++------++
                |+++++-++++
                |+++++-++++
                |LONDON;DELHI;ICELAND;ANKARA""".stripMargin

  val result0 = """+L++++++++
                  |+O++++++++
                  |+N++++++++
                  |+DELHI++++
                  |+O+++C++++
                  |+N+++E++++
                  |+++++L++++
                  |++ANKARA++
                  |+++++N++++
                  |+++++D++++""".stripMargin

  val test1 = """+-++++++++
                |+-++++++++
                |+-------++
                |+-++++++++
                |+-++++++++
                |+------+++
                |+-+++-++++
                |+++++-++++
                |+++++-++++
                |++++++++++
                |AGRA;NORWAY;ENGLAND;GWALIOR""".stripMargin

  val result1 = """+E++++++++
                  |+N++++++++
                  |+GWALIOR++
                  |+L++++++++
                  |+A++++++++
                  |+NORWAY+++
                  |+D+++G++++
                  |+++++R++++
                  |+++++A++++
                  |++++++++++""".stripMargin
}
