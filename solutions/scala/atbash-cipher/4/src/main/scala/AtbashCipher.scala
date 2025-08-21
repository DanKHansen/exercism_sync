object AtbashCipher:
   private def streamLine(s: String): String =
      s.toLowerCase.filter(_.isLetterOrDigit).map(c => if c.isLetter then ('a' + ('z' - c)).toChar else c)
   def encode(s: String): String =
      streamLine(s).grouped(5).mkString(" ")
   def decode(s: String): String =
      streamLine(s).mkString
