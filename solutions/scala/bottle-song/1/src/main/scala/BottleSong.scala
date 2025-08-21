object BottleSong:
   private val m = Map(
     10 -> "Ten",
     9 -> "Nine",
     8 -> "Eight",
     7 -> "Seven",
     6 -> "Six",
     5 -> "Five",
     4 -> "Four",
     3 -> "Three",
     2 -> "Two",
     1 -> "One",
     0 -> "No")
   def recite(startBottles: Int, takeDown: Int): String =
      val song =
         for s <- startBottles until startBottles - takeDown by -1
         yield s"${m(s)} green ${if s == 1 then "bottle" else "bottles"} hanging on the wall,\n" +
            s"${m(s)} green ${if s == 1 then "bottle" else "bottles"} hanging on the wall,\n" +
            s"And if one green bottle should accidentally fall,\n" +
            s"There'll be ${m(s - 1).toLowerCase} green ${
                  if s - 1 == 1 then "bottle" else "bottles"
               } hanging on the wall.\n"
      song.mkString("\n")
