package hackerrank.functionalprogramming

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

  val test3 = """+-++++++++
                |+-++-+++++
                |+-------++
                |+-++-++-++
                |+-++-++-++
                |+-++-++-++
                |++++-++-++
                |+--------+
                |++++++++++
                |----------
                |CALIFORNIA;LASVEGAS;NIGERIA;CANADA;TELAVIV;ALASKA""".stripMargin

  val result3 = """+C++++++++
                  |+A++T+++++
                  |+NIGERIA++
                  |+A++L++L++
                  |+D++A++A++
                  |+A++V++S++
                  |++++I++K++
                  |+LASVEGAS+
                  |++++++++++
                  |CALIFORNIA""".stripMargin

  val test0MostlyFull = """+L++++++++
               |+O++++++++
               |+N++++++++
               |+DELHI++++
               |+O+++-++++
               |+N+++-++++
               |+++++-++++
               |++ANKARA++
               |+++++-++++
               |+++++-++++
               |AGRA;NORWAY;ENGLAND;GWALIOR""".stripMargin
}
