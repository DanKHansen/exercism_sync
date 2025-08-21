case class DNA(s: String):
  val ok = "ACGT"
  val nucleotideCounts: Either[Int,Map[Char, Int]] = 
    if s.toList.distinct.diff(ok.toList).length == 0 then  
      Right(ok.map(c => c -> s.count(_ == c)).toMap)
    else Left(-1)
    