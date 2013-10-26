package scala99

object Questions1To10 {
  /**
   * Get the last element of a List.
   */
  def problem01LastElement[T](list: List[T]): T = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case x :: xs => problem01LastElement(xs)
  }

}