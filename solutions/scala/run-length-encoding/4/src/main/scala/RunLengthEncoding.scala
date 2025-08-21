object RunLengthEncoding:
   def encode(s: String): String =
      ("(.)\\1*".r findAllIn s).map(g => s"${if g.length > 1 then g.length else ""}${g.head}").mkString

   def decode(s: String): String =
      ("(\\d*)(\\D)".r findAllMatchIn s).map(m => m.group(2) * m.group(1).toIntOption.getOrElse(1)).mkString