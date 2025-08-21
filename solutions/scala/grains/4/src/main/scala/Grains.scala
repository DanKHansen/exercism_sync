object Grains:
   def square(n: Int): Option[BigInt] =
      if n >= 1 && n <= 64 then Some(BigInt(1) << (n - 1)) else None

   val total: BigInt = (BigInt(1) << 64) - 1
