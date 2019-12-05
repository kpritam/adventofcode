package day4

import java.util.regex.Pattern

object Part1 extends App {
  private val passwords1: List[String] = new Checker(235741, 706948).matchingPasswords

  println(passwords1.length)

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
    var last: Option[Int] = None
    var currentIndex      = 0
    val digits            = password.toCharArray.map(_.asDigit)

    digits
      .sliding(3)
      .map { xs =>
        val b = xs.toList match {
          case List(x, y, z) if x == y && y != z                                      => if (last.isDefined) last.get != x else true
          case List(x, y, z) if x != y && y == z && currentIndex + 3 == digits.length => true
          case List(x, y) if x == y                                                   => if (last.isDefined) last.get != x else true
          case List(x) if last.isDefined && last.get == x                             => true
          case _                                                                      => false
        }
        currentIndex += 1
        last = xs.headOption
        b
      }
      .exists(x => x)
  }
  def increasingOrder(password: String): Boolean = password.toSeq.sorted.unwrap == password
}
