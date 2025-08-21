object Raindrops:
   def convert(n: Int): String =
      val vs = Map(3 -> "i", 5 -> "a", 7 -> "o").filter(n % _._1 == 0).values.map(s => s"Pl${s}ng")
      if vs.isEmpty then n.toString else vs.mkString