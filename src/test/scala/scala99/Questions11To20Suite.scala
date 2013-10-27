package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Questions11To20._

@RunWith(classOf[JUnitRunner])
class Questions11To20Suite extends FunSuite {
  test("problem11: encodeModified: empty") {
    assert(encodeModified(List()) === List())
  }

  test("problem11: encodeModified: non-empty") {
    assert(encodeModified("aaaabcccdeeeff".toList) ===
      List((4, 'a'), 'b', (3, 'c'), 'd', (3, 'e'), (2, 'f')))
  }

  test("problem12: decode: empty") {
    assert(decode(List()) === List())
  }

  test("problem12: decode: non-empty") {
    assert(decode(List((4, 'a'), (1, 'b'), (3, 'c'), (1, 'd'), (3, 'e'), (2, 'f'))) ===
      "aaaabcccdeeeff".toList)
  }

  test("problem13: encodeDirect: empty") {
    assert(encodeDirect(List()) === List())
  }

  test("problem13: encodeDirect: non-empty") {
    assert(encodeDirect("aaaabcccdeeeff".toList) ===
      List((4, 'a'), 'b', (3, 'c'), 'd', (3, 'e'), (2, 'f')))
  }

  test("problem14: duplicate: empty") {
    assert(duplicate(List()) === List())
  }

  test("problem14: duplicate: non-empty") {
    assert(duplicate("abcdde".toList) ===
      "aabbccddddee".toList)
  }

  test("problem15: replicate: empty") {
    assert(replicate(List(), 2) === List())
  }

  test("problem15: replicate: non-empty") {
    assert(replicate("abc".toList, 3) === "aaabbbccc".toList)
  }

  test("problem15: replicate: 0 times") {
    assert(replicate("xyz".toList, 0) === List())
  }

  test("problem16: dropEveryN: empty") {
    assert(dropEveryN(List(), 2) === List())
  }

  test("problem16: dropEveryN: non-empty") {
    assert(dropEveryN("abcdefghijk".toList, 3) === "abdeghjk".toList)
  }

  test("problem17: split: empty") {
    assert(split("".toList, 2) === (List(), List()))
  }

  test("problem17: split: middle") {
    assert(split("abcdefgh".toList, 3) === ("abc".toList, "defgh".toList))
  }

  test("problem17: split: beginning") {
    assert(split("abcdefgh".toList, 0) === (List(), "abcdefgh".toList))
  }

  test("problem18: slice: empty") {
    assert(slice("".toList, 0, 2) === List())
  }

  test("problem18: slice: regular") {
    assert(slice("abcdefgh".toList, 2, 5) === "cde".toList)
  }

  test("problem18: slice: index out of bounds") {
    assert(slice("abc".toList, 1, 15) === "bc".toList)
  }

  test("problem19: rotate: empty") {
    assert(rotate("".toList, 2) === List())
  }

  test("problem19: rotate: positive n") {
    assert(rotate(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k), 3) ===
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("problem19: rotate: negative n") {
    assert(rotate(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k), -2) ===
      List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("problem19: rotate: index out of bounds") {
    assert(rotate("abc".toList, 4) === "abc".toList)
  }

  test("problem20: removeAt: empty") {
    intercept[NoSuchElementException] {
      removeAt(List(), 0)
    }
  }

  test("problem20: removeAt: regular") {
    assert(removeAt("abcdefgh".toList, 2) === ("abdefgh".toList, 'c'))
  }

  test("problem20: removeAt: index out of bounds") {
    intercept[NoSuchElementException] {
      removeAt("abcd".toList, 7)
    }
  }
}