object Sieve:
   def primes(n: Int): Seq[Int] = (2 to n).filterNot(i => (2 until i).exists(j => i % j == 0))
