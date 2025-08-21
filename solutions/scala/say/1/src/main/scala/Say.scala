object Say:
   private val units = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
   private val teens = Array(
     "ten",
     "eleven",
     "twelve",
     "thirteen",
     "fourteen",
     "fifteen",
     "sixteen",
     "seventeen",
     "eighteen",
     "nineteen")
   private val tens = Array("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
   private val thousands = Array("", "thousand", "million", "billion", "trillion")

   private def convert(num: Long): String =
      (0 until 5).toList
         .map(i => (num / math.pow(1000, i).toLong % 1000, thousands(i)))
         .collect { case (n, t) if n > 0 => convertLessThanThousand(n) + (if t.nonEmpty then s" $t" else "") }
         .reverse
         .mkString(" ")

   private def convertLessThanThousand(num: Long): String =
      val hundredsPart = if num >= 100 then s"${units((num / 100).toInt)} hundred" else ""
      val tensAndUnitsPart = convertTensAndUnits(num % 100)
      List(hundredsPart, tensAndUnitsPart).filter(_.nonEmpty).mkString(" ").trim

   private def convertTensAndUnits(num: Long): String = num match
      case n if n >= 20 => s"${tens((n / 10).toInt)}${if n % 10 > 0 then s"-${units((n % 10).toInt)}" else ""}"
      case n if n >= 10 => teens((n - 10).toInt)
      case _            => units(num.toInt)

   def inEnglish(number: Long): Option[String] = number match
      case n if n < 0 || n >= 1e12 => None
      case 0                       => Some("zero")
      case _                       => Some(convert(number))
