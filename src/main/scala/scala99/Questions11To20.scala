package scala99

import scala.annotation.tailrec

object Questions11To20 {
  /**
   * Problem 11
   *
   * Modified run-length encoding.
   *
   * Modify the result of problem P10 in such a way that if an element has no
   * duplicates it is simply copied into the result list. Only elements with
   * duplicates are transferred as (N, E) terms.
   *
   * Example:
   *   encodeModified("aaaabcccdeeeff".toList) ===
   *     List((4, 'a'), 'b', (3, 'c'), 'd', (3, 'e'), (2, 'f'))
   */
  def encodeModified[T](list: List[T]): List[Any] = {
    import Questions01To10.encode

    /**
     * Create the filtered list, but in a reversed representation.
     */
    @tailrec
    def filterEncoded(list: List[(Int, T)], acc: List[Any]): List[Any] = {
      list match {
        case Nil => acc
        case (x @ (c, elem)) :: xs =>
          if (c > 1) filterEncoded(xs, x :: acc)
          else if (c == 1) filterEncoded(xs, elem :: acc)
          else filterEncoded(xs, acc)
      }
    }

    val encodedList = encode(list)
    filterEncoded(encodedList, List()).reverse
  }

  /**
   * Problem 12
   *
   * Decode a run-length encoded list.
   *
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
   *
   * Example:
   *   decode(List((4, 'a'), (1, 'b'), (3, 'c'), (1, 'd'), (3, 'e'), (2, 'f'))) ===
   *     "aaaabcccdeeeff".toList
   */
  def decode[T](list: List[(Int, T)]): List[T] = {
    /**
     * Create the decoded list, but in a reversed representation.
     */
    @tailrec
    def decodeInt(list: List[(Int, T)], acc: List[T]): List[T] = {
      list match {
        case Nil => acc
        case (c, elem) :: xs =>
          if (c > 0) decodeInt((c - 1, elem) :: xs, elem :: acc)
          else decodeInt(xs, acc)
      }
    }
    decodeInt(list, List()).reverse
  }

  /**
   * Problem 13
   *
   * Run-length encoding of a list (direct solution).
   *
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
   * but only count them.
   * As in problem P11, simplify the result list by replacing the tuple (1, X) by X.
   *
   * Example:
   *   encodeDirect("aaaabcccdeeeff".toList) ===
   *     List((4, 'a'), 'b', (3, 'c'), 'd', (3, 'e'), (2, 'f'))
   */
  def encodeDirect[T](list: List[T]): List[Any] = {
    /**
     * Increase counter in the first element of a list.
     */
    def increaseCounterInFirst(list: List[Any]): List[Any] = {
      list match {
        case Nil => throw new IllegalArgumentException("No first element in empty list.")
        case (c: Int, x) :: xs => (c + 1, x) :: xs
        case x :: xs => (2, x) :: xs
      }
    }
    /**
     * Create the encoded list, but in a reversed representation.
     */
    @tailrec
    def encodeInt(list: List[T], acc: List[Any], last: Option[T]): List[Any] = {
      (list, last) match {
        case (Nil, _) => acc
        case (x :: xs, Some(y)) =>
          if (x == y) encodeInt(xs, increaseCounterInFirst(acc), Some(x))
          else encodeInt(xs, x :: acc, Some(x))
        case (x :: xs, None) =>
          encodeInt(xs, x :: acc, Some(x))
      }
    }
    encodeInt(list, List(), None).reverse
  }

  /**
   * Problem 14
   *
   * Duplicate the elements of a list.
   *
   * Example:
   *   duplicate("abcdde".toList) === "aabbccddddee".toList
   */
  def duplicate[T](list: List[T]): List[T] = {
    /**
     * Create the list with duplicated elements, but in a reversed representation.
     */
    @tailrec
    def duplicateInt(list: List[T], acc: List[T]): List[T] = {
      list match {
        case Nil => acc
        case x :: xs =>
          duplicateInt(xs, x :: x :: acc)
      }
    }
    duplicateInt(list, List()).reverse
  }

  /**
   * Problem 15
   *
   * Replicate the elements of a list a given number of times.
   *
   * Example:
   *   replicate("abc".toList, 3) === "aaabbbccc".toList
   */
  def replicate[T](list: List[T], count: Int): List[T] = {
    /**
     * Create the list with replicated elements, but in a reversed representation.
     */
    @tailrec
    def replicateInt(list: List[T], acc: List[T], count: Int, countCurrent: Int): List[T] = {
      list match {
        case Nil => acc
        case x :: xs if countCurrent > 0 =>
          replicateInt(list, x :: acc, count, countCurrent - 1)
        case x :: xs =>
          replicateInt(xs, acc, count, count)
      }
    }
    replicateInt(list, List(), count, count).reverse
  }

  /**
   * Problem 16
   *
   * Drop every Nth element from a list.
   *
   * Example:
   *   dropEveryN("abcdefghijk".toList, 3) === "abdeghjk".toList
   */
  def dropEveryN[T](list: List[T], n: Int): List[T] = {
    /**
     * Create the list with dropped elements, but in a reversed representation.
     */
    @tailrec
    def dropEveryNInt(list: List[T], acc: List[T], n: Int, currentN: Int): List[T] = {
      list match {
        case Nil => acc
        case x :: xs if currentN > 1 =>
          dropEveryNInt(xs, x :: acc, n, currentN - 1)
        case x :: xs =>
          dropEveryNInt(xs, acc, n, n)
      }
    }
    if (n < 1) throw new IllegalArgumentException
    else dropEveryNInt(list, List(), n, n).reverse
  }

  /**
   * Problem 17
   *
   * Split a list into two parts.
   *
   * The length of the first part is given. Use a Tuple for your result.
   *
   * Example:
   *   split("abcdefgh".toList, 3) === ("abc".toList, "defgh".toList)
   */
  def split[T](list: List[T], firstLength: Int): (List[T], List[T]) = {
    @tailrec
    def splitInt(list: List[T], acc: (List[T], List[T]), firstLength: Int, currentN: Int): (List[T], List[T]) = {
      list match {
        case Nil => acc
        case x :: xs =>
          if (currentN < firstLength)
            splitInt(xs, (x :: acc._1, acc._2), firstLength, currentN + 1)
          else
            splitInt(xs, (acc._1, x :: acc._2), firstLength, currentN + 1)
      }
    }
    val (first, second) = splitInt(list, (List(), List()), firstLength, 0)
    (first.reverse, second.reverse)
  }

  /**
   * Problem 18
   *
   * Extract a slice from a list.
   *
   * Given two indices, I and K, the slice is the list containing the elements
   * from and including the Ith element up to but not including the Kth element
   * of the original list.
   * Start counting the elements with 0.
   *
   * Example:
   *   slice("abcdefgh".toList, 2, 5) === "cde".toList
   */
  def slice[T](list: List[T], i: Int, k: Int): List[T] = {
    @tailrec
    def sliceInt(list: List[T], acc: List[T], i: Int, k: Int, currentN: Int): List[T] = {
      list match {
        case Nil => acc
        case x :: xs =>
          if (currentN < i)
            sliceInt(xs, acc, i, k, currentN + 1)
          else if (currentN < k)
            sliceInt(xs, x :: acc, i, k, currentN + 1)
          else
            acc
      }
    }
    sliceInt(list, List(), i, k, 0).reverse
  }

  /**
   * Problem 19
   *
   * Rotate a list N places to the left.
   *
   * Example:
   *   rotate("abcdefghijk".toList, 3) === List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
   */
  def rotate[T](list: List[T], n: Int): List[T] = {
    val splitPoint = if (n < 0) list.length + n
    else n
    val (first, second) = split(list, splitPoint)
    second ++ first
  }

  /**
   * Problem 20
   *
   * Remove the Kth element from a list.
   *
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   *
   * Example:
   *   removeAt("abcdefgh".toList, 2) === ("abdefgh".toList, 'c')
   */
  def removeAt[T](list: List[T], k: Int): (List[T], T) = {
    /**
     * Create the list without the removed element, but in a reversed representation.
     */
    @tailrec
    def removeAtInt[T](list: List[T], acc: List[T], k: Int, currentK: Int, kTh: Option[T]): (List[T], T) = {
      (list, kTh) match {
        case (Nil, None) => throw new NoSuchElementException
        case (Nil, Some(kThValue)) => (acc, kThValue)
        case (x :: xs, _) =>
          if (currentK < k || currentK > k) removeAtInt(xs, x :: acc, k, currentK + 1, kTh)
          else removeAtInt(xs, acc, k, currentK + 1, Some(x))
      }
    }
    val (newList, removed) = removeAtInt(list, List(), k, 0, None)
    (newList.reverse, removed)
  }
}