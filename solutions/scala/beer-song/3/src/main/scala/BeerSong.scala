object BeerSong {

  // First successful attempt
  /*  def recite(b: Int, v: Int): String = {
      var song: String = ""
      var i: Int = 1
      for (j <- b until (b - v) by -1) {
        if (v - i != 0) {
          song = song.concat(buildVerse(j)) + "\n"
          i = i + 1
        } else {
          song = song.concat(buildVerse(j))
        }
      }
      if (v == 100) print(song) else ()
      song
    }*/

  // 1. Refactored after inspiration
  /*  def recite(b: Int, v: Int): String = {
      val song: IndexedSeq[String] = for (i <- b until (b - v) by -1) yield {
        buildVerse(i)
      }
      song.mkString("\n")
    }*/

  // Final refactoring
  def recite(b: Int, v: Int): String = b.until(b - v, -1).map(n => buildVerse(n)).mkString("\n")

  def buildVerse(n: Int): String = {
    val genericOne: String = s"$n bottle${if (n > 1) "s" else ""} of beer on the wall, $n bottle${if (n > 1) "s" else ""} of beer."
    val genericTwo: String = s"Take one down and pass it around, ${n - 1} bottle${if (n - 1 > 1) "s" else ""} of beer on the wall."
    val thirdToLast: String = "Take it down and pass it around, no more bottles of beer on the wall."
    val secondToLast: String = "No more bottles of beer on the wall, no more bottles of beer."
    val last: String = "Go to the store and buy some more, 99 bottles of beer on the wall."

    n match {
      case 0 => s"$secondToLast\n$last\n"
      case 1 => s"$genericOne\n$thirdToLast\n"
      case _ => s"$genericOne\n$genericTwo\n"
    }
  }
}

