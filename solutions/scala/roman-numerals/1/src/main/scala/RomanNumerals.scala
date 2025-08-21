object RomanNumerals:
   def roman(n: Int): String =
      val m = List("", "M", "MM", "MMM")
      val c = List("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
      val x = List("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
      val i = List("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
      m(n / 1000) + c((n % 1000) / 100) + x((n % 100) / 10) + i(n % 10)
