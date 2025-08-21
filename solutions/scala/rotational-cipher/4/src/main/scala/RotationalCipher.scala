object RotationalCipher:
   def rotate(s: String, i: Int): String =
      def f(c: Char) = if c >= 'a' then 'a' else 'A'
      s.map(c => if c.isLetter then ((c - f(c) + i) % 26 + f(c)).toChar else c)
