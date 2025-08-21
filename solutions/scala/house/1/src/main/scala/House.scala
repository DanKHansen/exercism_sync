object House {
  val item: List[String] = List("house that Jack built.", "malt", "rat", "cat", "dog", "cow with the crumpled horn",
    "maiden all forlorn", "man all tattered and torn", "priest all shaven and shorn", "rooster that crowed in the morn",
    "farmer sowing his corn", "horse and the hound and the horn")
  val action: List[String] = List("lay in", "ate", "killed", "worried", "tossed", "milked", "kissed", "married", "woke",
    "kept", "belonged to")

  def recite(firstVerse: Int, lastVerse: Int): String = {
    val res: Seq[String] = for (i: Int <- firstVerse to lastVerse) yield {
      buildVerse(i)
    }
    res.mkString(" ")
  }

  def buildVerse(i: Int): String = {
    val firstLine: String = s"This is the ${item(i - 1)}"
    val secondLine: Seq[String] = for (x: Int <- i to 2 by -1) yield {
      s" that ${action(x - 2)} the ${item(x - 2)}"
    }
    i match {
      case 1 => firstLine
      case _ => firstLine + secondLine.mkString
    }
  }

}