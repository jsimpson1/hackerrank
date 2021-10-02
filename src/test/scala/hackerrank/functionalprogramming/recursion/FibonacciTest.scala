package hackerrank.functionalprogramming.recursion

import org.scalatest.funsuite.AnyFunSuite

class FibonacciTest extends AnyFunSuite {


  def testModule(n: Long, modulo: Long, expected: Long): Unit = {
    test(s"Fibonacci with modulo for ${n}") {
      assertResult(expected)(Fibonacci(n, modulo))
    }
  }

  val module: Long = math.pow(10, 8).toInt + 7

  testModule(0, module, 0)

  testModule(1, module, 1)

  testModule(5, module, 5)

  testModule(10, module, 55)

  testModule(100, module, 24278230)


}
