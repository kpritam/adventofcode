package day3

object Part1 {
  def run(wire1: List[String], wire2: List[String]): Int = {
    val intersectingPoints: List[Point] = Wire.from(wire1).intersect(Wire.from(wire2))

    intersectingPoints
      .map(p => Math.abs(p.x) + Math.abs(p.y))
      .filterNot(_ == 0)
      .min
  }
}
