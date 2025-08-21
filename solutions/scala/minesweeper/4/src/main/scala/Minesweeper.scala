object Minesweeper:
   def annotate(l: List[String]): List[String] =
      l.indices.map(r =>
            l(r).indices.map(c =>
                  if l(r)(c) == '*' then '*'
                  else
                     val n = (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (r + dx, c + dy))).count { case (x, y) =>
                        (x != r || y != c) && l.indices.contains(x) && l.head.indices.contains(y) && l(x)(
                          y) == '*'
                     }
                     if n == 0 then ' ' else n.toString.head)
               .mkString)
         .toList