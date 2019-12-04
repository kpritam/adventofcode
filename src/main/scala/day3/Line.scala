package day3

import java.lang.Integer.{max, min}
import day3.Direction.{D, L, R, U}

final case class Line(p1: Point, p2: Point, direction: Direction) {
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

  val steps: Int = direction.steps

  def stepsFromPoint(p: Point): Int = direction match {
    case _: L | _: R => Math.abs(x1 - p.x)
    case _: U | _: D => Math.abs(y1 - p.y)
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
