object Acronym:
   def abbreviate(phrase: String): String = phrase.split("[ ,-]+").map(_.head.toUpper).mkString
