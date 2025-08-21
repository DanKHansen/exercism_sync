case class DNA(s: String):
  val nucleotideCounts: Either[Int,Map[Char, Int]] = 
    if s.filterNot(_ == 'A').filterNot(_ == 'C').filterNot(_ =='G').filterNot(_ == 'T').isEmpty then  
      s match
        case str => Right(Map('A' -> str.count(_ == 'A'), 
                          'C' -> str.count(_ == 'C'), 
                          'G' -> str.count(_ == 'G'), 
                          'T' -> str.count(_ == 'T')))
        case _ => Left(0)
    else Left(0)
    