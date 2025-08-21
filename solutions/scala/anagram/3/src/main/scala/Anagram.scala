object Anagram:
   def findAnagrams(t: String, cs: List[String]): List[String] =
      cs.filter(c => c.toLowerCase.sorted == t.toLowerCase.sorted && !t.equalsIgnoreCase(c))