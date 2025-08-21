object Raindrops:
   def convert(n: Int): String =
      Seq(3 -> "Pling", 5 -> "Plang", 7 -> "Plong").collect { case (f, s) if n % f == 0 => s }.mkString match
         case ""     => n.toString
         case sounds => sounds
