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

  /**
   * Problem 25
   *
   * Generate a random permutation of the elements of a list.
   *
   * Example:
   *   randomPermute("abcde".toList) => List('c', 'b', 'd', 'e', 'a')
   */
  def randomPermute[T](list: List[T]): List[T] = {
    val len = list.length
    randomSelect(list, len)
  }

  /**
   * Problem 26
   *
   * Generate the combinations of K distinct objects chosen from the N elements of a list.
   *
   * In how many ways can a committee of 3 be chosen from a group of 12 people?
   * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known
   * binomial coefficient). For pure mathematicians, this result may be great.
   * But we want to really generate all the possibilities.
   *
   * Example:
   *   combinations(List('a, 'b, 'c), 2) === List(List('b, 'c), List('a, 'c), List('a, 'b))
   */
  def combinations[T](list: List[T], count: Int): List[List[T]] = {
    def combinationsInt(list: List[T], acc: List[List[T]], count: Int, current: Int, tmpList: List[T]): List[List[T]] = {
      // When we reached the count, add the gathered elements to acc.
      if (current == count) tmpList :: acc
      else
        list match {
          case Nil =>
            // If we reached the list's end without reaching count, then do not add tmpList.
            acc
          case x :: xs =>
            // Take both "left" and "right" paths in the "tree" of solutions
            // (left tree takes this element, the right tree doesn't take it).
            val withTaken = combinationsInt(xs, acc, count, current + 1, x :: tmpList)
            val withNotTaken = combinationsInt(xs, withTaken, count, current, tmpList)
            withNotTaken
        }
    }
    combinationsInt(list.reverse, List(), count, 0, List())
  }

  /**
   * Problem 28
   *
   * Sorting a list of lists according to length of sublists.
   *
   * a) We suppose that a list contains elements that are lists themselves.
   *    The objective is to sort the elements of the list according to their length.
   *    E.g. short lists first, longer lists later, or vice versa.
   *
   * Example:
   *   lsort(List(List('a', 'b', 'c', 'd'), List('a', 'b', 'c'), List('a'), List('a', 'b'))) ===
   *     List(List('a'), List('a', 'b'), List('a', 'b', 'c'), List('a', 'b', 'c', 'd'))
   */
  def lsort[T](list: List[List[T]]): List[List[T]] = {
    def lsortInt(list: List[(List[T], Int)]): List[(List[T], Int)] = {
      def mergeLists(xs: List[(List[T], Int)], ys: List[(List[T], Int)]): List[(List[T], Int)] = {
        (xs, ys) match {
          case (x :: xs1, y :: ys1) =>
            if (x._2 < y._2) x :: mergeLists(xs1, ys)
            else y :: mergeLists(xs, ys1)
          case (Nil, _) =>
            ys
          case (_, Nil) =>
            xs
        }
      }
      val n = list.length
      if (n == 1) list
      else {
        val (left, right) = list.splitAt(n / 2)
        val leftOrdered = lsortInt(left)
        val rightOrdered = lsortInt(right)
        mergeLists(leftOrdered, rightOrdered)
      }
    }
    val listWithLengths = list.map { x => (x, x.length) }
    lsortInt(listWithLengths).map { x => x._1 }
  }

  /**
   * Problem 28
   *
   * Sorting a list of lists according to length of sublists.
   *
   * b) Again, we suppose that a list contains elements that are lists themselves.
   *    But this time the objective is to sort the elements according to their length
   *    frequency; i.e. in the default, sorting is done ascendingly, lists with rare
   *    lengths are placed, others with a more frequent length come later.
   *
   * Example:
   *   lsortFreq(List(List('a', 'b', 'c', 'd'), List('a', 'b', 'c'), List('a'), List('a', 'b'), List('b'), List('c', 'd'), List('e'))) ===
   *     List(List('a', 'b', 'c'), List('a', 'b', 'c', 'd'), List('c', 'd'), List('a', 'b'), List('e'), List('b'), List('a'))
   */
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val listWithLengths = list.map { x => (x, x.length) }
    val listLengths = listWithLengths.foldLeft(Map[Int, Int]()) {
      case (z, (_, len)) =>
        z.get(len) match {
          case Some(oldLen) => z updated (len, z(len) + 1)
          case None => z updated (len, 1)
        }
    }
    def lsortFreqInt(list: List[(List[T], Int)]): List[(List[T], Int)] = {
      def mergeLists(xs: List[(List[T], Int)], ys: List[(List[T], Int)]): List[(List[T], Int)] = {
        (xs, ys) match {
          case (x :: xs1, y :: ys1) =>
            if (listLengths(x._2) < listLengths(y._2)) x :: mergeLists(xs1, ys)
            else y :: mergeLists(xs, ys1)
          case (Nil, _) =>
            ys
          case (_, Nil) =>
            xs
        }
      }
      val n = list.length
      if (n == 1) list
      else {
        val (left, right) = list.splitAt(n / 2)
        val leftOrdered = lsortFreqInt(left)
        val rightOrdered = lsortFreqInt(right)
        mergeLists(leftOrdered, rightOrdered)
      }
    }

    lsortFreqInt(listWithLengths).map { x => x._1 }
  }
}