object Bob {
  def response(statement: String): String = statement match {
    // Shouting -> all caps + not ends with ? + only letter
    case s if (s == s.toUpperCase && !s.endsWith("?")) && !s.filter(_.isLetter).isEmpty => "Whoa, chill out!"
    // Asking question -> not uppercase ending with ? + trimmed ends with ?
    case s if s != s.toUpperCase && s.trim.endsWith("?") || (s.filter(_.isLetter).isEmpty && s.endsWith("?")) => "Sure."
    // forceful question -> all caps + ending with ?
    case s if s == s.toUpperCase && s.endsWith("?") => "Calm down, I know what I'm doing!"
    // silence -> empty or multiple " "
    case s if s.isEmpty || s.trim.isEmpty => "Fine. Be that way!"
    // Only letters
    case _ => "Whatever."
  }
}
