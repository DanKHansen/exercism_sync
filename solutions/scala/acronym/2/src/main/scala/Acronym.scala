object Acronym:
   def abbreviate(phrase: String): String = phrase.split("[ -]").filterNot(_.isEmpty).map(_.head.toUpper).mkString