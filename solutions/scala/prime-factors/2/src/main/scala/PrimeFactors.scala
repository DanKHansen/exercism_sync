object PrimeFactors:

  def factors(n: Long): List[Int] =
    @annotation.tailrec
    def loop(n: Long, d: Int, acc: List[Int]): List[Int] = n match
      case 1               => acc.reverse
      case _ if n % d == 0 => loop(n / d, d, d :: acc)
      case _               => loop(n, d + 1, acc)

    loop(n, 2, Nil)