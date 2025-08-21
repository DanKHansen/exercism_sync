object Luhn:
   def valid(s: String): Boolean =
      s.filterNot(_.isSpaceChar).reverse.zipWithIndex.map { (d, i) =>
         val x = d.asDigit * (1 + i % 2)
         x - (if (x > 9) 9 else 0)
      }.sum % 10 == 0 && s.trim.length > 1