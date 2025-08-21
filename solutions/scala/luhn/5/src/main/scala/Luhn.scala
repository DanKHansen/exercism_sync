object Luhn:
   def valid(s: String): Boolean =
      s.filterNot(_.isSpaceChar)
         .reverse
         .zipWithIndex
         .map { case (d, i) =>
            if d.asDigit * (1 + i % 2) > 9 then d.asDigit * (1 + i % 2) - 9 else d.asDigit * (1 + i % 2)
         }
         .sum % 10 == 0 && s.trim.length > 1
