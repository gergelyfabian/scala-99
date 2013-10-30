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

  test("problem37: phiImproved(10)") {
    assert(phiImproved(10) === 4)
  }

  test("problem37: phiImproved(36)") {
    assert(phiImproved(36) === 12)
  }

  test("problem38: phiImproved(10090) === phi(10090)") {
    assert(phiImproved(10090) === phi(10090))
  }

  test("problem39: listPrimesInRange(10, 31)") {
    assert(listPrimesInRange(10, 31) === List(11, 13, 17, 19, 23, 29, 31))
  }

  test("problem40: goldbach(28)") {
    assert(goldbach(28) === (5, 23))
  }

  test("problem40: goldbach(100)") {
    assert(goldbach(100) === (3, 97))
  }

  test("problem41: goldbachList(10, 30)") {
    assert(goldbachList(10, 30) === List(
      (10, 3, 7),
      (12, 1, 11),
      (14, 1, 13),
      (16, 3, 13),
      (18, 1, 17),
      (20, 1, 19),
      (22, 3, 19),
      (24, 1, 23),
      (26, 3, 23),
      (28, 5, 23),
      (30, 1, 29)))
  }

  test("problem41: goldbachListLimited(180, 200, 50)") {
    goldbachListLimited(180, 200, 50) === List(
      (180, 53, 127),
      (182, 73, 109),
      (184, 53, 131),
      (186, 59, 127),
      (188, 61, 127),
      (190, 53, 137),
      (192, 53, 139),
      (194, 67, 127),
      (196, 59, 137),
      (198, 59, 139),
      (200, 61, 139))
  }
}