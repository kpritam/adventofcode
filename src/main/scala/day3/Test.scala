package day3

import day3.Input.{TestData, data1, data2, data3}

object Test extends App {

  private def runTest(testData: TestData): Unit = {
    val outputPart1 = Part1.run(testData.wire1, testData.wire2)
    val outputPart2 = Part2.run(testData.wire1, testData.wire2)
    assert(outputPart1 == testData.expectedResultPart1, s"Expected output = ${testData.expectedResultPart1} but Actual output = $outputPart1")
    assert(outputPart2 == testData.expectedResultPart2, s"Expected output = ${testData.expectedResultPart2} but Actual output = $outputPart2")
  }

  List(data1, data2, data3).foreach(runTest)
  println("All tests passed!")

  val outputPart1 = Part1.run(Input.mainInput1, Input.mainInput2)
  val outputPart2 = Part2.run(Input.mainInput1, Input.mainInput2)
  println("==========================")
  println(s"[Part 1] Final Answer = $outputPart1")
  println(s"[Part 2] Final Answer = $outputPart2")
  println("==========================")
}
