object FoodChain:
  private val what: List[String] =
    List("fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse")
  private val comment: List[String] = List(
    "It wriggled and jiggled and tickled inside her.",
    "How absurd to swallow a bird!",
    "Imagine that, to swallow a cat!",
    "What a hog, to swallow a dog!",
    "Just opened her throat and swallowed a goat!",
    "I don't know how she swallowed a cow!"
  )

  def recite(start: Int, end: Int): String =
    val res = for x <- start to end yield buildVerse(x)
    res.mkString

  private def buildVerse(i: Int): String =
    val firstLine = s"I know an old lady who swallowed a ${what(i - 1)}."
    lazy val commentLine = comment(i - 2)
    val runningLine =
      for x <- i to 2 by -1
      yield s"She swallowed the ${what(x - 1)} to catch the ${what(x - 2)}."
    val lastLine =
      s"I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
    var verse: List[String] = Nil
    i match
      case 1 => verse = firstLine :: lastLine :: verse
      case 2 =>
        verse =
          firstLine :: comment.head :: runningLine.mkString :: lastLine :: verse
      case 8 => verse = firstLine :: "She's dead, of course!\n\n" :: verse
      case _ =>
        verse = firstLine :: commentLine :: runningLine
          .dropRight(1)
          .mkString("\n")
          .dropRight(
            1
          ) + s" that ${comment.head.drop(3)}" :: runningLine.last :: lastLine :: verse
    verse.mkString("\n")
