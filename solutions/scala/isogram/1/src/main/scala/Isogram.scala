object Isogram:
   def isIsogram(s: String): Boolean = s.filter(_.isLetter).toLowerCase == s.filter(_.isLetter).toLowerCase.distinct
