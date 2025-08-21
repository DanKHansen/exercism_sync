object Sieve:
   def primes(n: Int): Seq[Int] =
      def isPrime(i: Int): Boolean =
         if i <= 1 then false
         else if i == 2 then true
         else !(2 until i).exists(n => i % n == 0)
      (1 to n).filter(isPrime)   
         
