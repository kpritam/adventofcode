package day3

import day3.Direction.{D, L, R, U}
import day3.Wire.Origin

final case class Wire(lines: List[Line]) {
  def intersect(other: Wire): List[Point] = {
    val linesCartesianProduct = for {
      l1 <- this.lines
      l2 <- other.lines
    } yield (l1, l2)

    linesCartesianProduct
      .flatMap {
        case (line1, line2) => line1.intersect(line2)
      }
      .filterNot(_ == Origin)
  }

  def steps(point: Point): Int = {
    def go(lines: List[Line]): Int =
      lines match {
        case Nil                            => 0
        case line :: _ if line.exist(point) => line.stepsFromPoint(point)
        case line :: xs                     => line.steps + go(xs)
      }

    go(lines)
  }
}

object Wire {
  val Origin: Point = Point(0, 0)

  def from(directions: List[String]): Wire = {
    def go(from: Point, directions: List[String]): List[Line] =
      directions match {
        case Nil => Nil
        case x :: xs =>
          val dir = x.head match {
            case 'U' => U(from, x.tail.toInt)
            case 'R' => R(from, x.tail.toInt)
            case 'D' => D(from, x.tail.toInt)
            case 'L' => L(from, x.tail.toInt)
            case d   => throw new RuntimeException(s"Invalid direction: $d provided")
          }
          Line(from, dir.point, dir) :: go(dir.point, xs)
      }

    Wire(go(Origin, directions))
  }
}
