object RunLengthEncoding:
   def encode(s: String): String =
      s.toList
         .foldRight(List.empty[(Char, Int)]) {
            case (c, (ch, cnt) :: tail) if c == ch => (ch, cnt + 1) :: tail
            case (c, acc)                          => (c, 1) :: acc
         }
         .map { case (ch, cnt) => if cnt > 1 then s"$cnt$ch" else ch.toString }
         .mkString

   def decode(s: String): String =
      s.toList
         .foldLeft(("", "")) {
            case ((acc, digitBuffer), c) if c.isDigit => (acc, digitBuffer + c)
            case ((acc, digitBuffer), ch)             =>
               val count = if digitBuffer.isEmpty then 1 else digitBuffer.toInt
               (acc + ch.toString * count, "")
         }
         ._1
