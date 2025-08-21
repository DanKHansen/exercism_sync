object Grains:
   def square(n: Int): Option[BigInt] =
      n match
         case x if x >= 1 && x <= 63 => Some(f(x))
         case y if y == 64           => Some(f(y) + 1)
         case _                      => None

   private def f(n: Int): BigInt = BigInt(math.pow(2, n - 1).toLong)

   val total: BigInt =
      val seq = for i <- 1 to 64 yield square(i) match
         case Some(value) => value
         case None        => BigInt(0)
      seq.sum
