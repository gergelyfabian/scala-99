package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Questions31To41._

@RunWith(classOf[JUnitRunner])
class Questions31To41Suite extends FunSuite {
  test("problem31: isPrime(2)") {
    assert(isPrime(2) === true)
  }

  test("problem31: isPrime(7)") {
    assert(isPrime(7) === true)
  }

  test("problem31: isPrime(6)") {
    assert(isPrime(6) === false)
  }

  test("problem31: isPrime(31)") {
    assert(isPrime(31) === true)
  }

  test("problem31: isPrime(3581)") {
    assert(isPrime(3581) === true)
  }

  test("problem31: isPrime(100000003)") {
    assert(isPrime(100000003) === false)
  }

  test("problem32: gcd(36, 63)") {
    assert(gcd(36, 63) === 9)
  }

  test("problem32: gcd(1071, 462)") {
    assert(gcd(1071, 462) === 21)
  }

  test("problem33: isCoprime(35, 64)") {
    assert(isCoprime(35, 64) === true)
  }

  test("problem33: isCoprime(12, 64)") {
    assert(isCoprime(12, 64) === false)
  }

  test("problem34: phi(10)") {
    assert(phi(10) === 4)
  }

  test("problem34: phi(36)") {
    assert(phi(36) === 12)
  }

  test("problem35: primeFactors(108)") {
    assert(primeFactors(108) === List(2, 2, 3, 3, 3))
  }

  test("problem35: primeFactors(315)") {
    assert(primeFactors(315) === List(3, 3, 5, 7))
  }

  test("problem36: primeFactorMultiplicity(108)") {
    assert(primeFactorMultiplicity(108) === List((2, 2), (3, 3)))
  }

  test("problem36: primeFactorMultiplicity(315)") {
    assert(primeFactorMultiplicity(315) === List((3, 2), (5, 1), (7, 1)))
  }
}