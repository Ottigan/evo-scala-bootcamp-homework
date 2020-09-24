package homework.basics

import scala.annotation.tailrec

object Basics extends App {
  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def gcd(a: Int, b: Int): Int = {
    @tailrec
    def helper(a: Int, b: Int, i: Int): Int = {
      if (a % i == 0 && b % i == 0) {
        i
      } else {
        helper(a, b, i - 1)
      }
    }

    if (a == 0 && b == 0) 0
    else if (a == 0) Math.abs(b)
    else if (b == 0) Math.abs(a)
    else if (Math.abs(a) >= Math.abs(b)) helper(a, b, b)
    else helper(a, b, a)
  }
}
