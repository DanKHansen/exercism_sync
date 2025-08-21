import scala.annotation.tailrec

object MatchingBrackets {
  private val openBrackets: List[Char] = List('[', '(', '{')
  private val closingBrackets: List[Char] = List(']', ')', '}')
  private var stack: List[Char] = Nil

  def isPaired(s: String): Boolean = {
    val validChars: List[Char] = openBrackets ++ closingBrackets
    val cs: String = s.filter(c => validChars.contains(c))
    // resetting the stack before every run
    stack = Nil
    check(cs)
  }

  private def isOpenBracket(c: Char): Boolean = {
    if (openBrackets contains c) true else false
  }

  private def isClosingBracket(c: Char): Boolean = {
    if (closingBrackets contains c) true else false
  }

  private def isMatchingOpenBracket(c: Char): Boolean = c match {
    case ']' if stack.nonEmpty && stack.last == '[' => true
    case ')' if stack.nonEmpty && stack.last == '(' => true
    case '}' if stack.nonEmpty && stack.last == '{' => true
    case _                                          => false
  }

  @tailrec
  private def check(s: String): Boolean = {
    s match {
      case x if x.isEmpty && stack.isEmpty  => true
      case x if x.isEmpty && stack.nonEmpty => false
      case _ =>
        if (s.isEmpty && stack.isEmpty) true
        else if (isOpenBracket(s.head)) {
          stack = stack :+ s.head
          check(s.tail)
        } else if (isClosingBracket(s.head)) {
          if (isMatchingOpenBracket(s.head)) {
            stack = stack.dropRight(1)
            check(s.tail)
          } else false
        } else false
    }
  }
}
