object Pangrams:
   def isPangram(input: String): Boolean = ('a' to 'z').forall(input.toLowerCase.contains)
