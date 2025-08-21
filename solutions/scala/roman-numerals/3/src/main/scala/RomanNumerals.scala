object RomanNumerals:
   def roman(n: Int): String = List(
     List("", "M", "MM", "MMM"),
     List("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
     List("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
     List("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
   ).zipWithIndex.map { case (l, i) => l((n / Math.pow(10, 3 - i).toInt) % 10) }.mkString
