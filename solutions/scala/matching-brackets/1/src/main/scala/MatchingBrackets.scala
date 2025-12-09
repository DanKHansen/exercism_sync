import scala.collection.mutable
import scala.util.boundary

object MatchingBrackets:
  def isPaired(brackets: String): Boolean =
    boundary:
      val pairs = Map(']' -> '[', '}' -> '{', ')' -> '(')
      val stack = mutable.Stack[Char]()
      for c <- brackets do
        if pairs.values.exists(_ == c) then stack.push(c)
        else if pairs.contains(c) then
          if stack.isEmpty || stack.pop() != pairs(c) then boundary.break(false)
      stack.isEmpty
