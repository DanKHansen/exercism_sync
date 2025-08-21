object RotationalCipher:
   def rotate(s: String, i: Int): String =
      val rot = if i > 25 then i - 26 else i
      for c <- s
      yield c match
         case c1 if (97 to 122).contains(c1) => if (c1 + rot) > 122 then (c1 + rot - 26).toChar else (c1 + rot).toChar
         case c2 if (65 to 90).contains(c2)  => if (c2 + rot) > 90 then (c2 + rot - 26).toChar else (c2 + rot).toChar
         case _                              => c
