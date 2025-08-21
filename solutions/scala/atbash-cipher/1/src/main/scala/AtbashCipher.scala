object AtbashCipher:
   private val mapping: Map[Char, Char] =
      ('a' to 'z').map(c => c -> ('z' - (c - 'a')).toChar).toMap ++ ('0' to '9').map(c => c -> c).toMap

   def encode(s: String): String = s.toLowerCase.filter(_.isLetterOrDigit).map(mapping).grouped(5).mkString(" ")
   def decode(s: String): String = s.toLowerCase.filter(_.isLetterOrDigit).map(mapping).mkString
