package day3

final case class Point(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"
}
