object Luhn:
   def valid(s: String): Boolean =
      s.filterNot(_.isSpaceChar).reverse.zipWithIndex
         .map { case (d, i) =>
            val n = d.asDigit * (1 + i % 2)
            if n > 9 then n - 9 else n
         }.sum % 10 == 0 && s.trim.length > 1
