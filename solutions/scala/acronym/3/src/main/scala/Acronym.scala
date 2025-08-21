object Acronym:
   def abbreviate(phrase: String): String = phrase.toUpperCase.split("\\s|-").flatMap(_.headOption).mkString