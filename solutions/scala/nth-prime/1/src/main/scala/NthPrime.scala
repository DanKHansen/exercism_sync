object NthPrime {

  lazy val primes: Seq[Int] = 2 #:: Stream.from(3, 2).filter(isPrime)

  def prime(x: Int): Option[Integer] = if (x == 0) None else Some(primes.take(x).last)

  def isPrime(n: Int): Boolean = primes.takeWhile(p => p * p <= n).forall(n % _ != 0)

}

