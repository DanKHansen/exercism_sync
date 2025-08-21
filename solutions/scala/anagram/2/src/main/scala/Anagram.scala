object Anagram:
   def findAnagrams(t: String, cs: List[String]): List[String] =
      cs.filterNot(t.equalsIgnoreCase).filter(_.toLowerCase.sorted == t.toLowerCase.sorted)
