package scala99

import scala.annotation.tailrec

object Questions31To41 {
  /**
   * Prime generator stream
   */
  val primes: Stream[Int] = {
    def primesInt(n: Int): Stream[Int] = {
      n #:: (primesInt(n + 1).filterNot { x => x % n == 0 })
    }
    1 #:: primesInt(2)
  }

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
  @tailrec
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
    @tailrec
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
    @tailrec
    def primeFactorMultiplicityInt(n: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val prime = (2 to n / 2).find { x => n % x == 0 }
      prime match {
        case Some(x) => primeFactorMultiplicityInt(n / x, increaseCounterInFirst(acc, x))
        case None => increaseCounterInFirst(acc, n)
      }
    }
    primeFactorMultiplicityInt(n, List()).reverse
  }

  /**
   * Problem 37
   *
   * Calculate Euler's totient function phi(m) (improved).
   *
   * See problem 34 for the definition of Euler's totient function. If the list of
   * the prime factors of a number m is known in the form of problem 36 then the
   * function phi(m) can be efficiently calculated as follows:
   *
   * Let ((p1, m1), (p2, m2), (p3, m3), ...) be the list of prime factors (and their
   * multiplicities) of a given number m.
   * Then phi(m) can be calculated with the following formula:
   *
   * phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
   *          (p2 - 1) * p2 ** (m2 - 1) *
   *          (p3 - 1) * p3 ** (m3 - 1) * ...
   *
   * Note that a ** b stands for the b'th power of a.
   *
   * Example:
   *   phiImproved(10) === 4
   */
  def phiImproved(m: Int): Int = {
    val factors = primeFactorMultiplicity(m)
    val totient = factors.foldLeft(1) {
      case (z, (p, m)) =>
        z * (p - 1) * math.pow(p, m - 1).toInt
    }
    totient
  }

  /**
   * Problem 38
   *
   * Compare the two methods of calculating Euler's totient function.
   * Use the solutions of problems P34 and P37 to compare the algorithms.
   * Try to calculate phi(10090) as an example.
   *
   * See unit tests.
   */

  /**
   * Problem 39
   *
   * A list of prime numbers.
   *
   * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
   *
   * Example:
   *   listPrimesInRange(10, 31) === List(11, 13, 17, 19, 23, 29, 31)
   */
  def listPrimesInRange(n: Int, m: Int): List[Int] = {
    primes.dropWhile(x => x < n).takeWhile(x => x <= m).toList
  }

  /**
   * Problem 40
   *
   * Goldbach's conjecture.
   *
   * Goldbach's conjecture says that every positive even number greater than 2
   * is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most
   * famous facts in number theory that has not been proved to be correct in the
   * general case. It has been numerically confirmed up to very large numbers
   * (much larger than Scala's Int can represent).
   *
   * Write a function to find the two prime numbers that sum up to a given even integer.
   *
   * Example:
   *   goldbach(28) = 5 + 23
   */
  def goldbach(n: Int): (Int, Int) = {
    if (n % 2 != 0) throw new IllegalArgumentException("Goldbach's conjecture is valid for even numbers only.")
    else {
      // Take all primes from the prime stream and for each check whether there is
      // another prime that sums up to n.
      // Take care to limit primes to less than or equal to (n - x), to remove infinite
      // checking of impossible combinations (we'd check for 1 + (1 | 2 | 3 | 5 | ...) endlessly).
      val primeFound = primes.takeWhile(_ < n).find { x =>
        primes.takeWhile(_ <= n - x).exists(y => x + y == n)
      }
      primeFound match {
        case None => throw new IllegalArgumentException("You just submitted a number that doesn't fill Goldbach's conjecture: " + n)
        case Some(num) => (num, n - num)
      }
    }
  }

  /**
   * Limited Goldbach's conjecture calculation
   *
   * Primes used need to be higher than or equal to a limit.
   *
   * Example:
   *   goldbachLimited(200, 50) === Some((61, 139))
   */
  def goldbachLimited(n: Int, limit: Int): Option[(Int, Int)] = {
    if (n % 2 != 0) throw new IllegalArgumentException("Goldbach's conjecture is valid for even numbers only.")
    else {
      val limitedPrimes = primes.dropWhile(x => x < limit)
      // Take all primes from the prime stream and for each check whether there is
      // another prime that sums up to n.
      // Take care to limit primes to less than or equal to (n - x), to remove infinite
      // checking of impossible combinations (we'd check for 1 + (1 | 2 | 3 | 5 | ...) endlessly).
      val primeFound = limitedPrimes.takeWhile(_ < n).find { x =>
        limitedPrimes.takeWhile(_ <= n - x).exists(y => x + y == n)
      }
      primeFound map { num =>
        (num, n - num)
      }
    }
  }

  /**
   * Problem 41
   *
   * A list of Goldbach compositions.
   *
   * Given a range of integers by its lower and upper limit, return a
   * list of all even numbers and their Goldbach composition.
   *
   * Example:
   *   goldbachList(10, 16) === List((10,3,7), (12,1,11), (14,1,13), (16,3,13))
   */
  def goldbachList(n: Int, m: Int): List[(Int, Int, Int)] = {
    if (n % 2 != 0 || m % 2 != 0) throw new IllegalArgumentException("Goldbach's conjecture is valid for even numbers only.")
    else
      (n to m by 2).map { x =>
        val gldbch = goldbach(x)
        (x, gldbch._1, gldbch._2)
      }.toList
  }

  /**
   * Problem 41b
   *
   * A list of Goldbach compositions (limited)
   *
   * In most cases, if an even number is written as the sum of two prime numbers,
   * one of them is very small.
   * Very rarely, the primes are both bigger than, say, 50.
   * Try to find out how many such cases there are in the range 2..3000.
   *
   * Example:
   *   goldbachListLimited(180, 184, 50) === List((180,53,127), (182,73,109), (184,53,131))
   */
  def goldbachListLimited(n: Int, m: Int, limit: Int): List[(Int, Int, Int)] = {
    if (n % 2 != 0 || m % 2 != 0) throw new IllegalArgumentException("Goldbach's conjecture is valid for even numbers only.")
    else
      (n to m by 2).flatMap { x =>
        goldbachLimited(x, limit).map(y => (x, y._1, y._2))
      }.toList
  }

}