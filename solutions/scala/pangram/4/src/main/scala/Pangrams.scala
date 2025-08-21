object Pangrams:
   def isPangram(input: String): Boolean = input.filter(_.isLetter).toLowerCase.distinct.length == 26
