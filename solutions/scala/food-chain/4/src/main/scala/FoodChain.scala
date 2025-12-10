object FoodChain:
  private val data = Vector("fly" -> "", "spider" -> "It wriggled and jiggled and tickled inside her.",
    "bird" -> "How absurd to swallow a bird!", "cat" -> "Imagine that, to swallow a cat!",
    "dog" -> "What a hog, to swallow a dog!", "goat" -> "Just opened her throat and swallowed a goat!",
    "cow" -> "I don't know how she swallowed a cow!", "horse" -> "She's dead, of course!")

  def recite(start: Int, end: Int): String = (start to end).map(verse).mkString

  private def verse(n: Int): String =
    val (name, comment) = data(n - 1)
    val start = s"I know an old lady who swallowed a $name."
    if n == 8 then s"$start\n$comment\n\n" else
      val chain = (n - 1 until 0 by -1).map(i =>
        s"She swallowed the ${data(i)._1} to catch the ${data(i - 1)._1}" +
        (if i == 2 then s" that ${data(1)._2.drop(3)}" else "."))
      (Vector(start) ++ Option(comment).filter(_.nonEmpty) ++ chain :+
        "I don't know why she swallowed the fly. Perhaps she'll die.\n\n").mkString("\n")
