package interview

import org.scalatest.funsuite.AnyFunSuite

object TimeStringSolution extends App {

  runTests

  def runTests: Unit = {
    new TimeTest().execute()
    new TimeStringTest().execute()
  }

}

object Time {

  def apply(seconds: Int): Time = {
    if ( seconds < 0) {
      throw new RuntimeException(s"cant create negative time")
    } else {
      val h = seconds/3600
      val m = seconds/60 % 60
      val s = seconds%60
      Time(h ,m, s)
    }
  }

}

case class Time(hour: Int, minute: Int, second: Int) {

  def asSeconds: Int = {
    val h = hour * 3600
    val m = minute * 60
    h + m + second
  }

  def add(t: Time): Time = {
    Time(t.asSeconds + asSeconds)
  }

  private def withLeadingZero(v: Int): String = {
    if ( v < 10) s"0$v" else v.toString
  }

  override def toString: String = {
    s"$hour:${withLeadingZero(minute)}:${withLeadingZero(second)}"
  }

}

object TimeString {

  def parse(str: String): List[Time] = {
    str
      .split(" ")
      .toList
      .map(v =>
        v.split(":") match {
          case Array(m, s) =>
            Time(0, m.toInt, s.toInt)
          case o =>
            throw new RuntimeException(s"parse -- unable to parse $o")
        }
      )
  }

  def sum(times: List[Time]): Time = {
    times
      .foldLeft(Time(0,0,0)){ (acc, time) =>
        acc.add(time)
      }
  }

  def solve(str: String): String = {
    sum(
      parse(str)
    )
      .toString
  }

}

class TimeTest extends AnyFunSuite {

  test("time apply") {

    assert(Time.apply(59) == Time(0,0,59))

    assert(Time.apply(61) == Time(0,1,1))

    assert(Time.apply(3661) == Time(1,1,1))


  }

}


class TimeStringTest extends AnyFunSuite {

  test("parse"){
    val input = "12:32 34:01 15:23 9:27 55:22 25:56"

    val expected: List[Time] = List(
      Time(0,12,32),
      Time(0,34,1),
      Time(0,15,23),
      Time(0,9,27),
      Time(0,55,22),
      Time(0,25,56),
    )

    val actual = TimeString.parse(input)

    assert(actual == expected)
  }

  test("sum") {
    val input = List(
      Time(0,12,32),
      Time(0,34,1),
      Time(0,15,23),
      Time(0,9,27),
      Time(0,55,22),
      Time(0,25,56),
    )

    val expected = Time(2,32,41)

    val actual = TimeString.sum(input)

    assert(actual == expected)
  }

  test("solve") {
    val input = "12:32 34:01 15:23 9:27 55:22 25:56"

    val expected = "2:32:41"

    val actual = TimeString.solve(input)

    assert(actual == expected)
  }


}
