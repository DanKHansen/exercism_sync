object RomanNumerals:
   def roman(n: Int): String =
      val (m, c, x, i) = (
        Array("", "M", "MM", "MMM"),
        Array("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
        Array("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
        Array("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
      m(n / 1000) + c((n % 1000) / 100) + x((n % 100) / 10) + i(n % 10)
