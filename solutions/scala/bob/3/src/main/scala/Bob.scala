object Bob {
  def response(statement: String): String = statement match {
    case s if s == s.toUpperCase && !s.endsWith("?") && !s.filter(_.isLetter).isEmpty => "Whoa, chill out!"
    case s if s != s.toUpperCase && s.trim.endsWith("?") || s.filter(_.isLetter).isEmpty && s.endsWith("?") => "Sure."
    case s if s == s.toUpperCase && s.endsWith("?") => "Calm down, I know what I'm doing!"
    case s if s.isEmpty || s.trim.isEmpty => "Fine. Be that way!"
    case _ => "Whatever."
  }
}
