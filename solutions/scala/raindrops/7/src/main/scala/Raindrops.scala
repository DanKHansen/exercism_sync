object Raindrops:
   def convert(n: Int): String =
      val s = Seq(3 -> "Pling", 5 -> "Plang", 7 -> "Plong").collect { case (f, s) if n % f == 0 => s }.mkString
      if s.isEmpty then n.toString else s
