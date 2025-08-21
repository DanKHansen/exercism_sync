case class DNA(s: String):
  val nucleotideCounts: Either[Int,Map[Char, Int]] = 
    if s.toList.distinct.diff("ACGT".toList).length == 0 then  
      Right(Map('A' -> s.count(_ == 'A'),
                'C' -> s.count(_ == 'C'),
                'G' -> s.count(_ == 'G'),
                'T' -> s.count(_ == 'T')))
    else Left(0)
    