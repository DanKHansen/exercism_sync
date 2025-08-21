import scala.annotation.tailrec

object RunLengthEncoding:
   def encode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String = cs match
         case Nil => acc
         case _   =>
            val (first, rest) = cs.span(_ == cs.head)
            loop(
              rest.dropWhile(_ == cs.head),
              acc + (if first.length > 1 then first.length.toString else "") + first.head)
      loop(s.toList, "")

   def decode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String = cs match
         case Nil => acc
         case _   =>
            val (digits, rest) = cs.span(_.isDigit)
            val count = if digits.isEmpty then 1 else digits.mkString.toInt
            loop(rest.drop(1), acc + rest.head.toString * count)
      loop(s.toList, "")
