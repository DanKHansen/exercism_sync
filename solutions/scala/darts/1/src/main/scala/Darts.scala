object Darts:
   def score(x: Double, y: Double): Int =
      val r = math.sqrt(x * x + y * y)
      r match
         case a if a > 5 & a <= 10 => 1
         case b if b > 1 & b <= 5  => 5
         case c if c <= 1          => 10
         case _                    => 0
