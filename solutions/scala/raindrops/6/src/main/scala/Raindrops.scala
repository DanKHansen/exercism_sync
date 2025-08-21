object Raindrops:
   def convert(n: Int): String =
      Seq(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")
        .collect { case (f, s) if n % f == 0 => s }
        .reduceOption(_ + _).getOrElse(n.toString)