package scala99

import scala.annotation.tailrec

object Questions21To28 {
  /**
   * Problem 21
   *
   * Insert an element at a given position into a list.
   *
   * Example:
   *   insertAt(List('a, 'b, 'c, 'd), 'new, 1) === List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt[T](list: List[T], elem: T, k: Int): List[T] = {
    /**
     * Create the list with the inserted element, but in a reversed representation.
     */
    @tailrec
    def insertAtInt[T](list: List[T], acc: List[T], elem: T, k: Int, currentK: Int, found: Boolean): List[T] =
      (list, k, found) match {
        case (Nil, 0, _) => List(elem)
        case (Nil, _, false) => throw new IndexOutOfBoundsException
        case (Nil, _, true) => acc
        case (x :: xs, _, _) =>
          if (currentK == k) insertAtInt(xs, x :: elem :: acc, elem, k, currentK + 1, true)
          else insertAtInt(xs, x :: acc, elem, k, currentK + 1, found)
      }
    insertAtInt(list, List(), elem, k, 0, false).reverse
  }

  /**
   * Problem 22
   *
   * Create a list containing all integers within a given range.
   *
   * Example:
   *   range(4, 9) === List(4, 5, 6, 7, 8, 9)
   */
  def range(start: Int, end: Int): List[Int] = {
    @tailrec
    def rangeInt(start: Int, end: Int, acc: List[Int], currentN: Int): List[Int] =
      if (currentN < start) rangeInt(start, end, acc, currentN + 1)
      else if (currentN <= end) rangeInt(start, end, currentN :: acc, currentN + 1)
      else acc
    if (start > end) throw new IndexOutOfBoundsException("Invalid range: " + start + " > " + end)
    else rangeInt(start, end, List(), 0).reverse
  }

  /**
   * Problem 23
   *
   * Extract a given number of randomly selected elements from a list.
   *
   * Example:
   *   randomSelect("abcdefg".toList, 3) => List(c, d, a)
   */
  def randomSelect[T](list: List[T], count: Int): List[T] = {
    import Questions11To20.removeAt
    import scala.util.Random

    val rand = new Random

    @tailrec
    def randomSelectInt(list: List[T], acc: List[T], length: Int, count: Int): List[T] = {
      // No more elements to be selected.
      if (count == 0) acc
      // More elements to be selected, but there is just one element remaining.
      else if (length == 1) list.head :: acc
      else {
        val removePos = rand.nextInt(length)
        val (newList, removedElem) = removeAt(list, removePos)
        randomSelectInt(newList, removedElem :: acc, length - 1, count - 1)
      }
    }
    val len = list.length
    if (len < count) throw new IllegalArgumentException("More elements requested than length of the list: " + count + " > " + len)
    else
      // Normally we should reverse the result list, but due to the random order
      // it does not really matter.
      randomSelectInt(list, List(), len, count)
  }

  /**
   * Problem 24
   *
   * Lotto: Draw N different random numbers from the set 1..M.
   *
   * Example:
   *   lotto(5, 35) => List(31, 22, 15, 32, 33)
   */
  def lotto(count: Int, rangeEnd: Int) = {
    if (count > rangeEnd) throw new IllegalArgumentException("More elements requested than set's length: " + count + " > " + rangeEnd)
    else {
      val numbers = range(1, rangeEnd)
      randomSelect(numbers, count)
    }
  }
}