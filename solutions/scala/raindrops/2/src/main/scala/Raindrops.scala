object Raindrops:
   def convert(n: Int): String =
      val factors = (n % 3, n % 5, n % 7)
      val sounds = Seq(
        if factors._1 == 0 then "Pling" else "",
        if factors._2 == 0 then "Plang" else "",
        if factors._3 == 0 then "Plong" else ""
      ).mkString
      if sounds.isEmpty then n.toString else sounds
