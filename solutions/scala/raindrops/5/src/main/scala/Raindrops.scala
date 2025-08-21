object Raindrops:
   def convert(n: Int): String = Map(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")
      .filter(n % _._1 == 0)
      .values
      .reduceOption(_ + _)
      .getOrElse(n.toString)
      .mkString