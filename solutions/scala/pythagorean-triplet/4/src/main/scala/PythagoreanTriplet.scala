object PythagoreanTriplet:

   def isPythagorean(t: (Int, Int, Int)): Boolean = t._1 * t._1 + t._2 * t._2 == t._3 * t._3

   def pythagoreanTriplets(n1: Int, n2: Int): Seq[(Int, Int, Int)] =
      for
         a <- n1 to n2
         b <- a + 1 to n2
         c = math.sqrt(a * a + b * b).toInt if isPythagorean(a, b, c) && c <= n2
      yield (a, b, c)

   def pythagoreanTripletsSum(n: Int): Seq[(Int, Int, Int)] =
      pythagoreanTriplets(1, n).filter(t => t._1 + t._2 + t._3 == n)
