object OcrNumbers:

   def convert(l: List[String]): String =
      val ocrs = Map(
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

      l.sliding(4, 4)
         .map { lines =>
            if lines.size != 4 || lines.exists(_.length % 3 != 0) then "?"
            else
               val digits = for (i <- 0 until lines.head.length / 3) yield
                  val cube = (0 until 4).map(j => lines(j).substring(i * 3, i * 3 + 3)).toList
                  ocrs.getOrElse(cube, "?")
               digits.mkString
         }
         .mkString(",")