package day3

import java.lang.Integer.{max, min}

import day3.Input.{TestData, data1, data2, data3}

final case class Point(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"
}

final case class Line(p1: Point, p2: Point) {
  private val x1: Int = p1.x
  private val x2: Int = p2.x
  private val y1: Int = p1.y
  private val y2: Int = p2.y

  private val constants: (Int, Int, Int) = {
    val a = y2 - y1
    val b = x1 - x2
    val c = a * x1 + b * y1
    (a, b, c)
  }

  def exist(p: Point): Boolean =
    ((p.x <= max(x1, x2)) && (p.x >= min(x1, x2))) && ((p.y <= max(y1, y2)) && (p.y >= min(y1, y2)))

  def intersect(other: Line): Option[Point] = {
    val (a1, b1, c1) = this.constants
    val (a2, b2, c2) = other.constants

    val determinant = a1 * b2 - a2 * b1

    if (determinant == 0) None // The lines are parallel. This is simplified
    else {
      val x     = (b2 * c1 - b1 * c2) / determinant
      val y     = (a1 * c2 - a2 * c1) / determinant
      val point = Point(x, y)
      if (this.exist(point) && other.exist(point)) Some(point)
      else None
    }
  }

}

sealed abstract class Direction(val point: Point) extends Product with Serializable
case class U(from: Point, y: Int)                 extends Direction(Point(from.x, from.y + y))
case class R(from: Point, x: Int)                 extends Direction(Point(from.x + x, from.y))
case class D(from: Point, y: Int)                 extends Direction(Point(from.x, from.y - y))
case class L(from: Point, x: Int)                 extends Direction(Point(from.x - x, from.y))

final case class Wire(lines: List[Line]) {
  def intersect(other: Wire): List[Point] = {
    val linesCartesianProduct = for {
      l1 <- this.lines
      l2 <- other.lines
    } yield (l1, l2)

    linesCartesianProduct.flatMap {
      case (line1, line2) => line1.intersect(line2)
    }
  }
}

object Wire {

  def from(directions: List[String]): Wire = {
    def go(from: Point, directions: List[String]): List[Line] =
      directions match {
        case Nil => Nil
        case x :: xs =>
          val nextPoint = x.head match {
            case 'U' => U(from, x.tail.toInt).point
            case 'R' => R(from, x.tail.toInt).point
            case 'D' => D(from, x.tail.toInt).point
            case 'L' => L(from, x.tail.toInt).point
            case d   => throw new RuntimeException(s"Invalid direction: $d provided")
          }
          Line(from, nextPoint) :: go(nextPoint, xs)
      }

    val seed = Point(0, 0)
    Wire(go(seed, directions))
  }

}

object Part1 extends App {

  private def run(wire1: List[String], wire2: List[String]): Int = {
    val intersectingPoints: List[Point] = Wire.from(wire1).intersect(Wire.from(wire2))

    intersectingPoints
      .map(p => Math.abs(p.x) + Math.abs(p.y))
      .filterNot(_ == 0)
      .min
  }

  private def runTest(testData: TestData): Unit = {
    val output = run(testData.wire1, testData.wire2)
    assert(output == testData.expectedResult, s"Expected output = ${testData.expectedResult} but Actual output = $output")
  }

  List(data1, data2, data3).foreach(runTest)
  println("All tests passed!")

  val output = run(Input.mainInput1, Input.mainInput2)
  println("==========================")
  println(s"Final Answer = $output")
  println("==========================")
}
