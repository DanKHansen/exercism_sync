object Bob:
  def response(statement: String): String =
    val s = statement.trim
    if s.isEmpty then return "Fine. Be that way!"
    val isYelling = s.exists(_.isLetter) && s.toUpperCase == s
    val isQuestion = s.last == '?'

    (isQuestion, isYelling) match
      case (true, true) => "Calm down, I know what I'm doing!"
      case (_, true)    => "Whoa, chill out!"
      case (true, _)    => "Sure."
      case _            => "Whatever."
