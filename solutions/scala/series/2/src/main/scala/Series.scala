object Series:
   def slices(i: Int, s: String): Seq[Seq[Int]] = s.sliding(i).map(_.map(_.asDigit)).toSeq