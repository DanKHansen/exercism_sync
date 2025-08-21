import java.util.InvalidPropertiesFormatException

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

      val lines = l.sliding(4, 4)
      val numbers = lines.map(l =>
         if l.size % 4 != 0 || l.forall(_.length % 3 != 0) then "?"
         else
            val tops = l.head.sliding(3, 3).toList
            val upperMiddles = l(1).sliding(3, 3).toList
            val lowerMiddles = l(2).sliding(3, 3).toList
            val bottoms = l(3).sliding(3, 3).toList
            val cubes = for i <- tops.indices yield tops(i) :: upperMiddles(i) :: lowerMiddles(i) :: bottoms(i) :: Nil
            cubes.map(c => ocrs.getOrElse(c, "?")).mkString)
      numbers.mkString(",")
