package day3

object Part2 {
  def minSteps(wire1: Wire, wire2: Wire, intersectionPoints: List[Point]): Int =
    intersectionPoints.map(combinedSteps(wire1, wire2, _)).min

  def combinedSteps(wire1: Wire, wire2: Wire, intersectionPoint: Point): Int =
    wire1.steps(intersectionPoint) + wire2.steps(intersectionPoint)

  def run(input1: List[String], input2: List[String]): Int = {
    val wire1                           = Wire.from(input1)
    val wire2                           = Wire.from(input2)
    val intersectingPoints: List[Point] = wire1.intersect(wire2)

    minSteps(wire1, wire2, intersectingPoints)
  }
}
