object Raindrops:
   def convert(n: Int): String =
      val vs = Map(3 -> 'i', 5 -> 'a', 7 -> 'o').filter(n % _._1 == 0).map(t => s"Pl${t._2}ng")
      if vs.isEmpty then n.toString else vs.mkString
