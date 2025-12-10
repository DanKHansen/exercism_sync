object Bob:
  def response(statement: String): String =
    statement.trim match
      case s if s.isEmpty => "Fine. Be that way!"
      case s if s.exists(_.isLetter) && !s.exists(_.isLower) =>
        if s.endsWith("?") then "Calm down, I know what I'm doing!"
        else "Whoa, chill out!"
      case s if s.endsWith("?") => "Sure."
      case _                    => "Whatever."