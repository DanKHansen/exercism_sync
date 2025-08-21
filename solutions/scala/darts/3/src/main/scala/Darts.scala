object Darts:
   def score(x: Double, y: Double): Int =
      Map(1 -> 10, 5 -> 5, 10 -> 1).find(math.hypot(x, y) <= _._1).map(_._2).getOrElse(0)
