package day1

object Part1 extends App {

  def calculatePerModule(mass: Int): Int = math.floorDiv(mass, 3) - 2

  def calculate(allModules: List[Int]): Int = allModules.map(calculatePerModule).sum

  println("==========================")
  println(s"Final Answer = ${calculate(Input.input)}")
  println("==========================")
}
