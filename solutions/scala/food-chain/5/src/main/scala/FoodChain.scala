object FoodChain:
  private val animals = Vector(
    "fly" -> "",
    "spider" -> "It wriggled and jiggled and tickled inside her.",
    "bird" -> "How absurd to swallow a bird!",
    "cat" -> "Imagine that, to swallow a cat!",
    "dog" -> "What a hog, to swallow a dog!",
    "goat" -> "Just opened her throat and swallowed a goat!",
    "cow" -> "I don't know how she swallowed a cow!",
    "horse" -> "She's dead, of course!"
  )

  def recite(start: Int, end: Int): String =
    (start to end).map(verse).mkString

  private def verse(n: Int): String =
    val (name, reaction) = animals(n - 1)
    val startLine = s"I know an old lady who swallowed a $name."

    if n == 8 then s"$startLine\n$reaction\n\n"
    else
      val chain = (n - 1 until 0 by -1).map { i =>
        val (predator, _) = animals(i)
        val (prey, _) = animals(i - 1)
        val end = if i == 2 then s" that ${animals(1)._2.drop(3)}" else "."
        s"She swallowed the $predator to catch the $prey$end"
      }

      val lines = Vector(startLine) ++
        Option.when(reaction.nonEmpty)(reaction) ++
        chain :+
        "I don't know why she swallowed the fly. Perhaps she'll die."

      lines.mkString("", "\n", "\n\n")
