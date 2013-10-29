package scala99

import scala.annotation.tailrec

object Questions31To41 {
  /**
   * Problem 31
   *
   * Determine whether a given integer number is prime.
   *
   * Example:
   *   isPrime(7) === true
   */
  def isPrime(num: Int): Boolean = {
    !((2 to num / 2).exists { x => num % x == 0 })
  }

  /**
   * Problem 32
   *
   * Determine the greatest common divisor of two positive integer numbers.
   * Use Euclid's algorithm.
   *
   * Example:
   *   gcd(36, 63) === 9
   */
  def gcd(a: Int, b: Int): Int = {
    if (a == 0) b
    else if (b == 0) a
    else if (a < b) gcd(b % a, a)
    else gcd(a % b, b)
  }

  /**
   * Problem 33
   *
   * Determine whether two positive integer numbers are coprime.
   *
   * Two numbers are coprime if their greatest common divisor equals 1.
   *
   * Example:
   *   isCoprime(35, 64) === true
   */
  def isCoprime(a: Int, b: Int): Boolean = {
    gcd(a, b) == 1
  }

  /**
   * Problem 34
   *
   * Calculate Euler's totient function phi(m).
   *
   * Euler's so-called totient function phi(m) is defined as the number of
   * positive integers r (1 <= r <= m) that are coprime to m.
   *
   * Example:
   *   phi(10) === 4
   */
  def phi(m: Int): Int = {
    (1 to m).count { x => isCoprime(x, m) }
  }

  /**
   * Problem 35
   *
   * Determine the prime factors of a given positive integer.
   *
   * Construct a flat list containing the prime factors in ascending order.
   *
   * Example:
   *   primeFactors(108) === List(2, 2, 3, 3, 3)
   */
  def primeFactors(n: Int): List[Int] = {
    def primeFactorsInt(n: Int, acc: List[Int]): List[Int] = {
      val prime = (2 to n / 2).find { x => n % x == 0 }
      prime match {
        case Some(x) => primeFactorsInt(n / x, x :: acc)
        case None => n :: acc
      }
    }
    primeFactorsInt(n, List()).reverse
  }

  /**
   * Problem 36
   *
   * Determine the prime factors of a given positive integer (2).
   * Construct a list containing the prime factors and their multiplicity.
   *
   * Example:
   *   primeFactorMultiplicity(315) === List((3,2), (5,1), (7,1))
   */
  def primeFactorMultiplicity(n: Int): List[(Int, Int)] = {
    def increaseCounterInFirst(list: List[(Int, Int)], num: Int): List[(Int, Int)] = {
      list match {
        case (x, c) :: xs if x == num => (x, c + 1) :: xs
        case _ => (num, 1) :: list
      }
    }
    def primeFactorMultiplicityInt(n: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val prime = (2 to n / 2).find { x => n % x == 0 }
      prime match {
        case Some(x) => primeFactorMultiplicityInt(n / x, increaseCounterInFirst(acc, x))
        case None => increaseCounterInFirst(acc, n)
      }
    }
    primeFactorMultiplicityInt(n, List()).reverse
  }
}