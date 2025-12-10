object FoodChain:
  private case class Animal(name: String, reaction: String = "")

  private val animals = Vector(
    Animal("fly"),
    Animal("spider", "It wriggled and jiggled and tickled inside her."),
    Animal("bird", "How absurd to swallow a bird!"),
    Animal("cat", "Imagine that, to swallow a cat!"),
    Animal("dog", "What a hog, to swallow a dog!"),
    Animal("goat", "Just opened her throat and swallowed a goat!"),
    Animal("cow", "I don't know how she swallowed a cow!"),
    Animal("horse", "She's dead, of course!")
  )

  def recite(start: Int, end: Int): String =
    (start to end).map(buildVerse).mkString

  private def buildVerse(verse: Int): String =
    val index = verse - 1
    val animal = animals(index)
    val startLine = s"I know an old lady who swallowed a ${animal.name}."

    if animal.name == "horse" then
      s"$startLine\n${animal.reaction}\n\n"
    else
      val parts = List.newBuilder[String]
      parts += startLine
      
      if animal.reaction.nonEmpty then parts += animal.reaction

      for i <- index until 0 by -1 do
        val predator = animals(i)
        val prey = animals(i - 1)
        val line = s"She swallowed the ${predator.name} to catch the ${prey.name}"
        
        // Special case: The spider description is reused in the chain
        if prey.name == "spider" then
          parts += s"$line that ${animals(1).reaction.drop(3)}"
        else
          parts += s"$line."

      parts += "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
      parts.result().mkString("\n")
