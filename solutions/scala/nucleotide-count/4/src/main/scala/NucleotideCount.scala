case class DNA(s: String):
  val cs = "ACGT"
  val nucleotideCounts: Either[Int,Map[Char, Int]] = 
    if s"^[${cs}]*$$".r.matches(s) then  
      Right(cs.map(c => c -> s.count(_ == c)).toMap)
    else Left(-1)
    