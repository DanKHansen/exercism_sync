object Luhn:
   def valid(s: String): Boolean =
      s.filterNot(_.isSpaceChar)
         .reverse
         .map(_.asDigit)
         .zipWithIndex
         .map((d, i) => if d * (1 + i % 2) > 9 then d * (1 + i % 2) - 9 else d * (1 + i % 2))
         .sum % 10 == 0 && s.trim.length > 1
