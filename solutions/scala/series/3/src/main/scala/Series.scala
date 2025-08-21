object Series:
   def slices(i: Int, s: String): Seq[Seq[Int]] = s.map(_.asDigit).sliding(i).toSeq