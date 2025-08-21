object Anagram:
   def findAnagrams(target: String, candidates: List[String]): List[String] =
      lazy val lT = target.toLowerCase
      candidates
         .filter(_.length == target.length)
         .filter(_.toLowerCase.forall(lT.contains(_)))
         .filter(_.toLowerCase != lT)
         .filter(_.toLowerCase.permutations.contains(lT))
