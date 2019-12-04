package day3

sealed abstract class Direction(val point: Point, val steps: Int) extends Product with Serializable

object Direction {
  final case class U(from: Point, override val steps: Int) extends Direction(Point(from.x, from.y + steps), steps)
  final case class R(from: Point, override val steps: Int) extends Direction(Point(from.x + steps, from.y), steps)
  final case class D(from: Point, override val steps: Int) extends Direction(Point(from.x, from.y - steps), steps)
  final case class L(from: Point, override val steps: Int) extends Direction(Point(from.x - steps, from.y), steps)
}
