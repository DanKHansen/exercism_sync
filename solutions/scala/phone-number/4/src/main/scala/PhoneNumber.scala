object PhoneNumber:
   def clean(s: String): Option[String] =
      "(?<=^1?)[2-9]\\d{2}[2-9]\\d{6}$".r.findFirstIn(s.filter(_.isDigit))
