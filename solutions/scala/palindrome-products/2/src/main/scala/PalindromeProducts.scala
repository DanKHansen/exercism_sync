case class PalindromeProducts(i: Int, j: Int):
   private lazy val l = (for a <- i to j; b <- a to j; if a * b == (a * b).toString.reverse.toInt
   yield (a * b, (a, b))).groupMap(_._1)(_._2)

   val smallest: Option[(Int, Set[(Int, Int)])] = l.minByOption(_._1).map((p, pairs) => (p, pairs.toSet))
   val largest: Option[(Int, Set[(Int, Int)])] = l.maxByOption(_._1).map((p, pairs) => (p, pairs.toSet))
