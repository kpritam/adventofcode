package day1

object Part2 extends App {

  def calculatePerModule(mass: Int): Int = {
    val rounded = math.floorDiv(mass, 3) - 2
    rounded match {
      case _ if rounded <= 0 => 0
      case _ if rounded < 3  => rounded
      case _                 => rounded + calculatePerModule(rounded)
    }
  }

  def calculate(allModules: List[Int]): Int = allModules.map(calculatePerModule).sum

  println("==========================")
  println(s"Test Answer = ${calculatePerModule(100756)}")
  println(s"Final Answer = ${calculate(Input.input)}")
  println("==========================")
}
