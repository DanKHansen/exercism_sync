object Sieve:
   def primes(n: Int): Seq[Int] = (2 to n).filter(i => (2 until i).forall(j => i % j != 0))