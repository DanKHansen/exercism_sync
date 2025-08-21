object PrimeFactors {

  def factors(n: Long): List[Long] = n match {
    case 1 => Nil
    case _ => buildList(n)
  }

  private def buildList(x: Long, prime: Int = 2): List[Long] = if (prime * prime > x) List(x) else
    x % prime match {
      case 0 => prime :: buildList(x / prime, prime)
      case _ => buildList(x, prime + 1)
    }

}