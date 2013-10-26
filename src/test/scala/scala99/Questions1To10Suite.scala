package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Questions1To10._

@RunWith(classOf[JUnitRunner])
class Questions1To10Suite extends FunSuite {

  test("problem01 empty list") {
    intercept[NoSuchElementException] {
      problem01LastElement(List())
    }
  }

  test("problem01 singleton list") {
    assert(problem01LastElement(List(67)) === 67)
  }

  test("problem01 short list") {
    assert(problem01LastElement(List(1, 4, 9, 16, 25, 36)) === 36)
  }

  test("problem01 long list") {
    assert(problem01LastElement(List(1000, 1, 4, 9, 16, 25, 36, 58, 34, 73, 99, 134, 382, 234, 999, 2)) === 2)
  }

}