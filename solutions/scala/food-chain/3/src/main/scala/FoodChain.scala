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
      val reaction = if animal.reaction.isEmpty then Nil else List(animal.reaction)
      
      val chain = (index until 0 by -1).map { i =>
        val predator = animals(i)
        val prey = animals(i - 1)
        val base = s"She swallowed the ${predator.name} to catch the ${prey.name}"
        if prey.name == "spider" then 
          s"$base that ${animals(1).reaction.drop(3)}"
        else 
          s"$base."
      }.toList

      val endLine = "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
      
      (startLine :: reaction ::: chain ::: List(endLine)).mkString("\n")
