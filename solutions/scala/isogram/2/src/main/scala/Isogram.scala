object Isogram:
   def isIsogram(s: String): Boolean =
      val ls = s.filter(_.isLetter).toLowerCase
      ls.toSet.size == ls.length
