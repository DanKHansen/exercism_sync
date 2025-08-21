object Sieve:
   def primes(n: Int): IndexedSeq[Int] = 2 to n filter (i => 2 until i forall (i % _ != 0))