object Diamond:
   def rows(c: Char): List[String] =
      val diamond = for (x <- 'A' to c) yield
         val spaces = " " * (c - x)
         val middle = if x == 'A' then "" else " " * (2 * (x - 'A') - 1)
         spaces + x + middle + (if x == 'A' then "" else x) + spaces
      diamond.toList ++ diamond.reverse.tail
