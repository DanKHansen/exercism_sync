object CryptoSquare:
   def ciphertext(s: String): String =
      val n = s.filter(_.isLetterOrDigit).toLowerCase
      val r = math.ceil(math.sqrt(n.length)).toInt
      if n.isEmpty then ""
      else n.grouped(r).toList.map(_.padTo(r, ' ')).transpose.map(_.mkString).mkString(" ")