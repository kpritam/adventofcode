package day2

object Part1 extends App {

  @scala.annotation.tailrec
  def run(currentOpCodePos: Int, input: List[Int]): List[Int] = {
    input match {
      case Nil => Nil
      case xs =>
        xs(currentOpCodePos) match {
          case 1  => run(currentOpCodePos + 4, add(input, currentOpCodePos))
          case 2  => run(currentOpCodePos + 4, mul(input, currentOpCodePos))
          case 99 => input
          case x  => throw new RuntimeException(s"opcode [$x] is invalid")
        }
    }
  }

  private def add(input: List[Int], currentOpCodePos: Int) = executeOp(input, currentOpCodePos, _ + _)
  private def mul(input: List[Int], currentOpCodePos: Int) = executeOp(input, currentOpCodePos, _ * _)

  private def executeOp(input: List[Int], currentOpCodePos: Int, op: (Int, Int) => Int) = {
    val firstElmPos = input(currentOpCodePos + 1)
    val sndElmPos   = input(currentOpCodePos + 2)
    val resultPos   = input(currentOpCodePos + 3)
    val firstElm    = input(firstElmPos)
    val sndElm      = input(sndElmPos)

    input.updated(resultPos, op(firstElm, sndElm))
  }

  println("==========================")
  println(s"Test 1 = ${run(0, List(1, 0, 0, 0, 99))}")
  println(s"Test 2 = ${run(0, List(2, 3, 0, 3, 99))}")
  println(s"Test 3 = ${run(0, List(2, 4, 4, 5, 99, 0))}")
  println(s"Test 4 = ${run(0, List(1, 1, 1, 4, 99, 5, 6, 0, 99))}")
  println(s"Final Answer = ${run(0, Input.input)}")
  println("==========================")
}
