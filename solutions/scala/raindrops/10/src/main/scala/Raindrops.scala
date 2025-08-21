object Raindrops:
   def convert(n: Int): String =
      Map(3 -> 'i', 5 -> 'a', 7 -> 'o')
         .filter(n % _._1 == 0)
         .map(t => s"Pl${t._2}ng")
         .reduceOption(_+_)
         .getOrElse(n.toString)
