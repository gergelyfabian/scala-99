package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Questions21To28._

@RunWith(classOf[JUnitRunner])
class Questions21To28Suite extends FunSuite {
  test("problem21: insertAt: empty") {
    assert(insertAt(List(), "elem", 0) === List("elem"))
  }

  test("problem21: insertAt: empty with out of bounds") {
    intercept[IndexOutOfBoundsException] {
      insertAt(List(), "elem", 1)
    }
  }

  test("problem21: insertAt: regular") {
    assert(insertAt(List('a, 'b, 'c, 'd), 'new, 1) === List('a, 'new, 'b, 'c, 'd))
  }

  test("problem21: insertAt: index out of bounds") {
    intercept[IndexOutOfBoundsException] {
      insertAt("abcd".toList, 'x', 7)
    }
  }

  test("problem22: range: singleton") {
    assert(range(2, 2) === List(2))
  }

  test("problem22: range: regular") {
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  test("problem22: range: invalid arguments") {
    intercept[IndexOutOfBoundsException] {
      range(5, 4)
    }
  }

  test("problem23: randomSelect: zero elements") {
    val origList = "abcdefg".toList
    val selectedList = randomSelect(origList, 0)
    assert(selectedList === List())
  }

  test("problem23: randomSelect: subset") {
    val origList = "abcdefg".toList
    for (i <- 1 to 5) {
      val selectedList = randomSelect(origList, 3)
      assert(selectedList.length === 3)
      val set = selectedList.toSet
      // Check there were no duplicated elements.
      assert(set.size === 3)
      assert(set.subsetOf(origList.toSet))
    }
  }

  test("problem23: randomSelect: full set") {
    val origList = "abcdefg".toList
    val len = origList.length
    val selectedList = randomSelect(origList, len)
    assert(selectedList.length === len)
    assert(selectedList.toSet === origList.toSet)
  }

  test("problem23: randomSelect: invalid arguments") {
    val origList = "abcdefg".toList
    intercept[IllegalArgumentException] {
      randomSelect(origList, 13)
    }
  }

  test("problem24: lotto: subset") {
    val top = 35
    val rangeVal = range(1, 35).toSet
    for (i <- 1 to 5) {
      val selectedList = lotto(5, top)
      assert(selectedList.length === 5)
      val set = selectedList.toSet
      // Check there were no duplicated elements.
      assert(set.size === 5)
      assert(set.subsetOf(rangeVal))
    }
  }

  test("problem24: lotto: full set") {
    val top = 35
    val rangeVal = range(1, top).toSet
    val len = rangeVal.size
    val selectedList = lotto(top, top)
    assert(selectedList.length === len)
    val set = selectedList.toSet
    assert(set === rangeVal)
  }

  test("problem24: lotto: invalid arguments") {
    intercept[IllegalArgumentException] {
      lotto(12, 10)
    }
  }

  test("problem25: randomPermute") {
    val origList = "abcdefg".toList
    val len = origList.length
    for (i <- 1 to 5) {
      val selectedList = randomPermute(origList)
      assert(selectedList.length === len)
      assert(selectedList.toSet === origList.toSet)
    }
  }

  test("problem26: combinations: (4,2)") {
    val origList = "abcd".toList
    assert(combinations(origList, 2).toSet ===
      Set(List('c', 'd'), List('b', 'd'), List('b', 'c'), List('a', 'd'), List('a', 'c'), List('a', 'b')))
  }

  test("problem26: combinations: (3,3)") {
    val origList = "abc".toList
    assert(combinations(origList, 3).toSet === Set(origList))
  }

  test("problem28: lsort") {
    assert(lsort(List(List('a', 'b', 'c', 'd'), List('a', 'b', 'c'), List('a'), List('a', 'b'))) ===
      List(List('a'), List('a', 'b'), List('a', 'b', 'c'), List('a', 'b', 'c', 'd')))
  }

  test("problem28: lsortFreq") {
    assert(lsortFreq(List(List('a', 'b', 'c', 'd'), List('a', 'b', 'c'), List('a'), List('a', 'b'), List('b'), List('c', 'd'), List('e'))) ===
      List(List('a', 'b', 'c'), List('a', 'b', 'c', 'd'), List('c', 'd'), List('a', 'b'), List('e'), List('b'), List('a')))
  }
}