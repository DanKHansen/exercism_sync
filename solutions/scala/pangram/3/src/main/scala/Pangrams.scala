object Pangrams:
   def isPangram(input: String): Boolean = input.filter(_.isLetter).map(_.toLower).distinct.length == 26
