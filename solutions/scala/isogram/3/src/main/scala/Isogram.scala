object Isogram:
   def isIsogram(s: String): Boolean =
      val lc = s.filter(_.isLetter).toLowerCase
      lc == lc.distinct
