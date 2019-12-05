package day4

import java.util.regex.Pattern

import scala.collection.mutable

object Part1 extends App {
  private val passwords: List[String] = new Checker(235741, 706948).matchingPasswords

  passwords.foreach(println)
  println(passwords.length)

}

class Checker(rangeFrom: Int, rangeTo: Int) {
  private val twoAdjacentDigitsMatcher = Pattern.compile(".*([\\d])\\1.*")

  def check(password: String): Option[String] =
    if (isSixDigit(password) && withinRange(password) && twoAdjacentDigitsStrict(password) && increasingOrder(password))
      Some(password)
    else None

  val matchingPasswords: List[String] = (rangeFrom to rangeTo).flatMap { p =>
    check(p.toString)
  }.toList

  def isSixDigit(password: String): Boolean        = password.length == 6
  def withinRange(password: String): Boolean       = password.toInt > rangeFrom && password.toInt < rangeTo
  def twoAdjacentDigits(password: String): Boolean = twoAdjacentDigitsMatcher.matcher(password).matches()
  def twoAdjacentDigitsStrict(password: String): Boolean = {
    val digits = password.toCharArray.map(_.asDigit).toList
    val stack  = mutable.Stack.empty[Int]
    var flag   = false

    def popPush(elm: Int) = {
      stack.pop()
      stack.push(elm)
    }

    def clearPush(elm: Int) = {
      stack.clear()
      stack.push(elm)
    }

    digits.foreach { d =>
      if (stack.isEmpty) stack.push(d)
      else if (stack.size == 1 && stack.top == d) stack.push(d)
      else if (stack.size == 1 && stack.top != d) popPush(d)
      else if (stack.size == 2 && stack.top == d) stack.push(d)
      else if (stack.size == 2 && stack.top != d) flag = true
      else if (stack.size == 3 && stack.top != d) clearPush(d)
    }

    stack.size == 2 || flag
  }
  def increasingOrder(password: String): Boolean = password.toSeq.sorted.unwrap == password
}
