object PythagoreanTriplet:

   def isPythagorean(t: (Int, Int, Int)): Boolean = t._1 * t._1 + t._2 * t._2 == t._3 * t._3

   def pythagoreanTriplets(n1: Int, n2: Int): Seq[(Int, Int, Int)] =
      for
         a <- n1 to n2
         b <- a + 1 to n2
         c <- a + 1 to n2 if isPythagorean(a, b, c)
      yield (a, b, c)

   def pythagoreanTripletsSum(n: Int): Seq[(Int, Int, Int)] =
      for
         a <- 1 to n
         b <- a + 1 to n
         c = n - a - b if isPythagorean(a, b, c)
      yield (a, b, c)
