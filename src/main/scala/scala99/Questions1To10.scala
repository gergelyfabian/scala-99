package scala99

import scala.annotation.tailrec

object Questions1To10 {
  /**
   * Problem 01
   *
   * Find the last element of a List.
   *
   * Example:
   *   last(List(1, 4, 9, 16, 25, 36)) === 36
   */
  @tailrec
  def last[T](list: List[T]): T = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case _ :: xs => last(xs)
  }

  /**
   * Problem 02
   *
   * Find the last but one element of a List.
   *
   * Example:
   *   lastButOne(List(1, 2, 3)) === 2
   */
  @tailrec
  def lastButOne[T](list: List[T]): T = list match {
    case Nil => throw new NoSuchElementException
    case _ :: Nil => throw new NoSuchElementException
    case x :: _ :: Nil => x
    case _ :: xs => lastButOne(xs)
  }

  /**
   * Problem 03
   *
   * Find the N'th element of a List.
   *
   * The first element of a List is by convention element 0.
   *
   * Example:
   *   nth(2, List(1, 4, 9, 10)) === 9
   */
  @tailrec
  def nth[T](n: Int, list: List[T]): T =
    list match {
      case Nil => throw new NoSuchElementException
      case x :: xs =>
        if (n == 0) x
        else nth(n - 1, xs)
    }

  /**
   * Problem 04
   *
   * Find the number of elements of a List.
   *
   * Example:
   *   length(List(1, 2)) === 2
   */
  def length[T](list: List[T]): Int = {
    @tailrec
    def tailRecLength(list: List[T], len: Int): Int =
      list match {
        case Nil => len
        case _ :: xs => tailRecLength(xs, len + 1)
      }
    tailRecLength(list, 0)
  }

  /**
   * Problem 05
   *
   * Reverse a list.
   *
   * Example:
   *   reverse(List(1, 2, 3)) === List(3, 2, 1)
   */
  def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def tailRecReverse(list: List[T], acc: List[T]): List[T] =
      list match {
        case Nil => acc
        case x :: xs => tailRecReverse(xs, x :: acc)
      }
    tailRecReverse(list, List())
  }

  /**
   * Problem 06
   *
   * Find out whether a list is a palindrome.
   *
   * Example:
   *   isPalindrome(List(1, 2, 3, 2, 1)) === true
   *   isPalindrome(List(1, 2, 3)) === false
   */
  def isPalindrome[T](list: List[T]): Boolean = {
    @tailrec
    def listsEqual(xs: List[T], ys: List[T]): Boolean = {
      (xs, ys) match {
        case (x :: xs1, y :: ys1) =>
          if (x == y) listsEqual(xs1, ys1)
          else false
        case (Nil, Nil) => true
        // This is the case if the lists have unequal length.
        case _ => false
      }
    }
    val reversedList = reverse(list)
    listsEqual(list, reversedList)
  }

  /**
   * Problem 07
   *
   * Flatten a nested list structure.
   *
   * Transform a list, possibly holding lists as elements into a 'flat'
   * list by replacing each list with its elements (recursively).
   *
   * Example:
   *   flatten(List(1, 2, List(3, 4), List(5, List(6, 7, 8), 9), 10)) ===
   *     List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   */
  def flatten(list: List[Any]): List[Any] = {
    /**
     * Concat two lists together where the first list's elements will
     * be inserted in reverse order
     */
    @tailrec
    def concatReverse(list1: List[Any], list2: List[Any]): List[Any] = list1 match {
      case Nil => list2
      case x :: xs =>
        concatReverse(xs, x :: list2)
    }
    list match {
      case Nil => List()
      case ((x: List[Any]) :: xs) =>
        // I could have used "++" operator here instead of reversing and calling concatReverse:
        // flatten(x) ++ flatten(xs)
        concatReverse(reverse(flatten(x)), flatten(xs))
      case (x :: xs) =>
        x :: flatten(xs)
    }
  }

  /**
   * Problem 08
   *
   * Eliminate consecutive duplicates of list elements.
   *
   * If a list contains repeated elements they should be replaced with
   * a single copy of the element. The order of the elements should not
   * be changed.
   *
   * Example:
   *   compress("aaaabbbccddddde".toList) === "abcde".toList
   */
  def compress[T](list: List[T]): List[T] = {
    /**
     * Create the compressed list, but in a reversed representation.
     */
    @tailrec
    def compressInt(list: List[T], acc: List[T], last: Option[T]): List[T] = {
      (list, last) match {
        case (Nil, _) => acc
        case (x :: xs, Some(y)) =>
          if (x == y) compressInt(xs, acc, Some(x))
          else compressInt(xs, x :: acc, Some(x))
        case (x :: xs, None) =>
          compressInt(xs, x :: acc, Some(x))
      }
    }
    reverse(compressInt(list, List(), None))
  }

  /**
   * Problem 09
   *
   * Pack consecutive duplicates of list elements into sublists.
   *
   * If a list contains repeated elements they should be placed in separate sublists.
   *
   * Example:
   *   pack("aaaabbcccdeeeff".toList) ===
   *     List(
   *       List('a', 'a', 'a', 'a'),
   *       List('b', 'b'),
   *       List('c', 'c', 'c'),
   *       List('d'),
   *       List('e', 'e', 'e'),
   *       List('f', 'f')
   *     )
   */
  def pack[T](list: List[T]): List[List[T]] = {
    /**
     * Add a new element to the first element of a list (which is itself a list).
     */
    def addToInternalList(list: List[List[T]], newElement: T): List[List[T]] = {
      list match {
        case Nil => List(List(newElement))
        case x :: xs => (newElement :: x) :: xs
      }
    }
    /**
     * Create the packed list, but in a reversed representation.
     *
     * This can be seen as a variation of compressInt.
     */
    @tailrec
    def packInt(list: List[T], acc: List[List[T]], last: Option[T]): List[List[T]] = {
      (list, last) match {
        case (Nil, _) => acc
        case (x :: xs, Some(y)) =>
          if (x == y) packInt(xs, addToInternalList(acc, x), Some(x))
          else packInt(xs, List(x) :: acc, Some(x))
        case (x :: xs, None) =>
          packInt(xs, List(x) :: acc, Some(x))
      }
    }
    reverse(packInt(list, List(), None))
  }

  /**
   * Problem 10
   *
   * Run-length encoding of a list.
   *
   * The original description says:
   * "Use the result of problem P09 to implement the so-called run-length encoding
   * data compression method. Consecutive duplicates of elements are encoded as
   * tuples (N, E) where N is the number of duplicates of the element E."
   *
   * We'll code the solution from ground up instead (better performance).
   */
  def encode[T](list: List[T]): List[(Int, T)] = {
    /**
     * We could do the following:
     *
     *   val packedList = pack(list)
     *   packedList.map { x => (x.length, x.head) }
     *
     * But it's more demonstrative to code the solution from ground up (and also
     * has better performance).
     */

    /**
     * Increase counter in the first element of a list (which is itself a tuple).
     */
    def increaseCounterInFirst(list: List[(Int, T)]): List[(Int, T)] = {
      list match {
        case Nil => throw new IllegalArgumentException("No first element in empty list.")
        case (c, x) :: xs => (c + 1, x) :: xs
      }
    }
    /**
     * Create the encoded list, but in a reversed representation.
     *
     * This can be seen as a variation of packInt.
     */
    @tailrec
    def encodeInt(list: List[T], acc: List[(Int, T)], last: Option[T]): List[(Int, T)] = {
      (list, last) match {
        case (Nil, _) => acc
        case (x :: xs, Some(y)) =>
          if (x == y) encodeInt(xs, increaseCounterInFirst(acc), Some(x))
          else encodeInt(xs, (1, x) :: acc, Some(x))
        case (x :: xs, None) =>
          encodeInt(xs, (1, x) :: acc, Some(x))
      }
    }
    reverse(encodeInt(list, List(), None))
  }
}