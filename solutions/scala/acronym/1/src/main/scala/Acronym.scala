object Acronym:
   def abbreviate(phrase: String): String = phrase.split(" ").flatMap(_.split("-")).map(_.head.toUpper).mkString
