object Diamond:
   def rows(c: Char): List[String] =
      val upperHalf = (0 to c - 'A').map { i =>
         val spaces = " " * (c - 'A' - i)
         val char = ('A' + i).toChar
         val middle = if i == 0 then "" else " " * (2 * i - 1)
         spaces + char + middle + (if i == 0 then "" else char) + spaces
      }
      upperHalf.toList ++ upperHalf.dropRight(1).reverse
