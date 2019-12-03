package day2

object Part2 extends App {

  val allCombinations = (for {
    x <- 0 to 99
    y <- 0 to 99
  } yield (x, y)).toList

  def updatedInput(noun: Int, verb: Int) =
    Input.input.updated(1, noun).updated(2, verb)

  val (result, noun, verb) = allCombinations
    .map {
      case (noun, verb) =>
        val input  = updatedInput(noun, verb)
        val result = Part1.run(currentOpCodePos = 0, input)
        (result.head, noun, verb)
    }
    .find {
      case (result, _, _) => result == 19690720
    }
    .getOrElse(throw new RuntimeException("No valid combination found!"))

  println(s"result = $result")
  println(s"noun = $noun")
  println(s"verb = $verb")
  println(s"100 * noun + verb = ${100 * noun + verb}")
}
