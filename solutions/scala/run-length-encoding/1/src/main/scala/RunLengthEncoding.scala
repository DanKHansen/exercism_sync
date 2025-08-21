import scala.annotation.tailrec

object RunLengthEncoding:
   def encode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String =
         if cs.isEmpty then acc
         else
            val first: List[Char] = cs.takeWhile(_ == cs.head)
            val l: String =
               if first.length > 1 then acc.concat(first.length.toString.concat(cs.head.toString))
               else acc.concat(cs.head.toString)
            loop(if first.isEmpty then cs.tail else cs.dropWhile(_ == cs.head), l)

      loop(s.toList, "").mkString

   def decode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String =
         if cs.isEmpty then acc
         else
            val i: String = cs.takeWhile(_.isDigit).mkString
            val t: List[Char] = cs.drop(i.length)
            loop(
              cs.drop(i.length).dropWhile(_ == t.head),
              acc.concat(t.head.toString * { if i.isEmpty then 1 else i.toInt }))

      loop(s.toList, "")
