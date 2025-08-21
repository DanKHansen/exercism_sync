object RotationalCipher:
   def rotate(s: String, i: Int): String =
      val rot = i % 26
      s.map {
         case c if c.isLower => (if c + rot > 'z' then c + rot - 26 else c + rot).toChar
         case c if c.isUpper => (if c + rot > 'Z' then c + rot - 26 else c + rot).toChar
         case c              => c
      }
