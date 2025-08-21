object OcrNumbers:
   private val ocrs = Map(
     List(" _ ", "| |", "|_|", "   ") -> "0",
     List("   ", "  |", "  |", "   ") -> "1",
     List(" _ ", " _|", "|_ ", "   ") -> "2",
     List(" _ ", " _|", " _|", "   ") -> "3",
     List("   ", "|_|", "  |", "   ") -> "4",
     List(" _ ", "|_ ", " _|", "   ") -> "5",
     List(" _ ", "|_ ", "|_|", "   ") -> "6",
     List(" _ ", "  |", "  |", "   ") -> "7",
     List(" _ ", "|_|", "|_|", "   ") -> "8",
     List(" _ ", "|_|", " _|", "   ") -> "9"
   )

   def convert(lines: List[String]): String =
      lines.sliding(4, 4).map {
            case g if g.size == 4 && g.forall(_.length % 3 == 0) =>
               (0 until g.head.length / 3).map(i => ocrs.getOrElse(g.map(_.substring(i * 3, i * 3 + 3)), "?")).mkString
            case _                                               => "?"
         }.mkString(",")