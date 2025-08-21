object RomanNumerals:
   def roman(n: Int): String =
      val rns = Seq(
        Seq("", "M", "MM", "MMM"),
        Seq("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
        Seq("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
        Seq("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
      )
      rns.head(n / 1000) + rns(1)((n % 1000) / 100) + rns(2)((n % 100) / 10) + rns(3)(n % 10)
