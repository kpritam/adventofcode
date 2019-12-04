package day3

object Part2 {
  def minSteps(wire1: Wire, wire2: Wire, intersectionPoints: List[Point]): Int =
    intersectionPoints.map(steps(wire1, wire2, _)).min

  def steps(wire1: Wire, wire2: Wire, intersectionPoint: Point): Int =
    steps(wire1, intersectionPoint) + steps(wire2, intersectionPoint)

  def steps(wire: Wire, intersectionPoint: Point): Int = {
    def go(lines: List[Line]): Int =
      lines match {
        case Nil                                        => 0
        case line :: _ if line.exist(intersectionPoint) => line.stepsFromPoint(intersectionPoint)
        case line :: xs                                 => line.steps + go(xs)
      }

    go(wire.lines)
  }

  def run(input1: List[String], input2: List[String]): Int = {
    val wire1                           = Wire.from(input1)
    val wire2                           = Wire.from(input2)
    val intersectingPoints: List[Point] = wire1.intersect(wire2)

    minSteps(wire1, wire2, intersectingPoints)
  }
}
