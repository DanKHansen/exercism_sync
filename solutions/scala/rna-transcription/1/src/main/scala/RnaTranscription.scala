object RnaTranscription:
   def toRna(s: String): Option[String] =
      Option(s.map(Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')))
