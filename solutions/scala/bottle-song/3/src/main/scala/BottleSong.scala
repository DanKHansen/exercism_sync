object BottleSong:
   private val l = Seq("No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
   def recite(s: Int, t: Int): String =
     (for b <- s until s - t by -1
        yield s"${l(b)} green bottle${if b == 1 then "" else "s"} hanging on the wall,\n" * 2 +
              s"And if one green bottle should accidentally fall,\n" +
              s"There'll be ${l(b - 1).toLowerCase} green bottle${if b - 1 == 1 then "" else "s"} hanging on the wall.\n")
     .mkString("\n")