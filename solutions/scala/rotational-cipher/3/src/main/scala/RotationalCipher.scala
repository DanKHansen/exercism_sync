object RotationalCipher:
   def rotate(s: String, i: Int): String = s.map {
      case c if c.isLower => ((c - 'a' + i % 26) % 26 + 'a').toChar
      case c if c.isUpper => ((c - 'A' + i % 26) % 26 + 'A').toChar
      case c              => c
   }