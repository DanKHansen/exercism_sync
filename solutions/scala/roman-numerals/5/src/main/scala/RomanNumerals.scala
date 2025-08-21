object RomanNumerals:
   def roman(n: Int): String = Array(
     Array("", "M", "MM", "MMM"),
     Array("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
     Array("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
     Array("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
   ).zipWithIndex.map { case (arr, idx) => arr((n / Math.pow(10, 3 - idx).toInt) % 10) }.mkString
