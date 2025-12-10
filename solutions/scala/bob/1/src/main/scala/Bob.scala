object Bob:

  /*
"Sure." This is his response if you ask him a question, such as "How are you?" The convention used for questions is that it ends with a question mark.
"Whoa, chill out!" This is his answer if you YELL AT HIM. The convention used for yelling is ALL CAPITAL LETTERS.
"Calm down, I know what I'm doing!" This is what he says if you yell a question at him.
"Fine. Be that way!" This is how he responds to silence. The convention used for silence is nothing, or various combinations of whitespace characters.
"Whatever." This is what he answers to anything else.

   */

  def response(statement: String): String =
    if statement.trim.isEmpty then return "Fine. Be that way!"
    val isYelling =
      statement.toUpperCase == statement && statement.exists(_.isLetter)
    val isQuestion = statement.trim.endsWith("?")

    (isYelling, isQuestion) match
      case (true, true)  => "Calm down, I know what I'm doing!"
      case (true, false) => "Whoa, chill out!"
      case (false, true) => "Sure."
      case _             => "Whatever."
