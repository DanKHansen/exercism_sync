object Pangrams:
   def isPangram(input: String): Boolean = ('a' to 'z').toSet.subsetOf(input.toLowerCase.toSet)
