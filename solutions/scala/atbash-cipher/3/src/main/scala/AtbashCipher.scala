object AtbashCipher:
   private def mapChar(c: Char): Char = if c.isLetter then ('a' + ('z' - c)).toChar else c
   def encode(s: String): String = decode(s).grouped(5).mkString(" ")
   def decode(s: String): String = s.toLowerCase.filter(_.isLetterOrDigit).map(mapChar).mkString
