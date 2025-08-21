object Luhn:
   def valid(s: String): Boolean =
      s.filterNot(_.isSpaceChar).reverse.zipWithIndex
         .map(d => (d._1.asDigit * (1 + d._2 % 2) + (if d._1.asDigit * (1 + d._2 % 2) > 9 then -9 else 0)) % 10)
         .sum % 10 == 0 && s.trim.length > 1