object BeerSong {

  def recite(b: Int, v: Int): String = {
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
  }

  def buildVerse(n: Int): String = {
    val genericOne: String = s"$n bottle${if (plural(n)) "s" else ""} of beer on the wall, $n bottle${if (plural(n)) "s" else ""} of beer.\n"
    val genericTwo: String = s"Take one down and pass it around, ${n - 1} bottle${if (plural(n - 1)) "s" else ""} of beer on the wall.\n"
    val thirdToLast: String = "Take it down and pass it around, no more bottles of beer on the wall.\n"
    val secondToLast: String = "No more bottles of beer on the wall, no more bottles of beer.\n"
    val last: String = "Go to the store and buy some more, 99 bottles of beer on the wall.\n"

    n match {
      case 0 => s"$secondToLast$last"
      case 1 => s"$genericOne$thirdToLast"
      case _ => s"$genericOne$genericTwo"
    }
  }

  def plural(i: Int): Boolean = i > 1

}

