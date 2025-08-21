object Raindrops:
   def convert(n: Int): String =
      val factors: (Int, Int, Int) = (n % 3, n % 5, n % 7)
      factors match
         case (0, i, j) if i != 0 & j != 0 => "Pling"
         case (i, 0, j) if i != 0 & j != 0 => "Plang"
         case (i, j, 0) if i != 0 & j != 0 => "Plong"
         case (0, 0, i) if i != 0          => "PlingPlang"
         case (0, i, 0) if i != 0          => "PlingPlong"
         case (i, 0, 0) if i != 0          => "PlangPlong"
         case (0, 0, 0)                    => "PlingPlangPlong"
         case _                            => n.toString
