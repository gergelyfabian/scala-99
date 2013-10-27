package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Questions01To10._

@RunWith(classOf[JUnitRunner])
class Questions01To10Suite extends FunSuite {

  test("problem01: last: empty list") {
    intercept[NoSuchElementException] {
      last(List())
    }
  }

  test("problem01: last: singleton list") {
    assert(last(List(67)) === 67)
  }

  test("problem01: last: short list") {
    assert(last(List(1, 4, 9, 16, 25, 36)) === 36)
  }

  test("problem01: last: long list") {
    assert(last(List(1000, 1, 4, 9, 16, 25, 36, 58, 34, 73, 99, 134, 382, 234, 999, 2)) === 2)
  }

  test("problem02: lastButOne: empty list") {
    intercept[NoSuchElementException] {
      lastButOne(List())
    }
  }

  test("problem02: lastButOne: singleton list") {
    intercept[NoSuchElementException] {
      lastButOne(List(52))
    }
  }

  test("problem02: lastButOne: short list") {
    assert(lastButOne(List(1, 4, 9, 16, 25, 36)) === 25)
  }

  test("problem02: lastButOne: long list") {
    assert(lastButOne(List(1000, 1, 4, 9, 16, 25, 36, 58, 34, 73, 99, 134, 382, 234, 999, 2)) === 999)
  }

  test("problem03: nth: empty list 0th") {
    intercept[NoSuchElementException] {
      nth(0, List())
    }
  }

  test("problem03: nth: empty list 1st") {
    intercept[NoSuchElementException] {
      nth(1, List())
    }
  }

  test("problem03: nth: out of bounds") {
    intercept[NoSuchElementException] {
      nth(1, List(52))
    }
  }

  test("problem03: nth: list first") {
    assert(nth(0, List(1, 4, 9)) === 1)
  }

  test("problem03: nth: list last") {
    assert(nth(4, List(1, 4, 9, 10, 11)) === 11)
  }

  test("problem04: length: empty list") {
    assert(length(List()) === 0)
  }

  test("problem04: length: singleton list") {
    assert(length(List(57)) === 1)
  }

  test("problem04: length: longer list") {
    assert(length(List(1, 2, 3, 4, 5, 6, 10, 20, 40)) === 9)
  }

  test("problem05: reverse: empty list") {
    assert(reverse(List()) === List())
  }

  test("problem05: reverse: singleton list") {
    assert(reverse(List(1)) === List(1))
  }

  test("problem05: reverse: longer list") {
    assert(reverse("abrakadabra".toList) === "arbadakarba".toList)
  }

  test("problem06: isPalindrome: empty list") {
    assert(isPalindrome(List()) === true)
  }

  test("problem06: isPalindrome: singleton list") {
    assert(isPalindrome(List(22)) === true)
  }

  test("problem06: isPalindrome: even length list") {
    assert(isPalindrome(List(1, 2, 2, 1)) === true)
  }

  test("problem06: isPalindrome: odd length list") {
    assert(isPalindrome("ammaiamma".toList) === true)
  }

  test("problem06: isPalindrome: not palindrome") {
    assert(isPalindrome(List(1, 2, 4, 5)) === false)
  }

  test("problem07: flatten: empty") {
    assert(flatten(List()) === List())
  }

  test("problem07: flatten: flat") {
    assert(flatten(List(1, 4, 8)) === List(1, 4, 8))
  }

  test("problem07: flatten: nested") {
    assert(flatten(List(1, 2, List(3, 4), List(5, List(6, 7, 8), 9), 10)) === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("problem08: compress: empty") {
    assert(compress(List()) === List())
  }

  test("problem08: compress: without duplicates") {
    assert(compress("abcxklu".toList) === "abcxklu".toList)
  }

  test("problem08: compress: with duplicates") {
    assert(compress("aaaabbbccddddde".toList) === "abcde".toList)
  }

  test("problem09: pack: empty") {
    assert(pack(List()) === List())
  }

  test("problem09: pack: without duplicates") {
    assert(pack("abcxklu".toList) === List(List('a'), List('b'), List('c'), List('x'), List('k'), List('l'), List('u')))
  }

  test("problem09: pack: with duplicates") {
    assert(pack("aaaabbcccdeeeff".toList) ===
      List(
        List('a', 'a', 'a', 'a'),
        List('b', 'b'),
        List('c', 'c', 'c'),
        List('d'),
        List('e', 'e', 'e'),
        List('f', 'f')))
  }

  test("problem10: encode: empty") {
    assert(encode(List()) === List())
  }

  test("problem10: encode: without duplicates") {
    assert(encode("abcxklu".toList) === List((1, 'a'), (1, 'b'), (1, 'c'), (1, 'x'), (1, 'k'), (1, 'l'), (1, 'u')))
  }

  test("problem10: encode: with duplicates") {
    assert(encode("aaaabbcccdeeeff".toList) ===
      List((4, 'a'), (2, 'b'), (3, 'c'), (1, 'd'), (3, 'e'), (2, 'f')))
  }
}