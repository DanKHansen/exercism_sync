object Raindrops:
   def convert(n: Int): String =
      val s = Map(3 -> "Pling", 5 -> "Plang", 7 -> "Plong").filter(n % _._1 == 0).values
      if s.isEmpty then n.toString else s.mkString
