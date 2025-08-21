object Minesweeper:
   def annotate(grid: List[String]): List[String] =
      grid.indices
         .map(r =>
            grid(r).indices
               .map(c =>
                  if grid(r)(c) == '*' then '*'
                  else
                     val n = (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (r + dx, c + dy))).count { case (x, y) =>
                        (x != r || y != c) && grid.indices.contains(x) && grid.head.indices.contains(y) && grid(x)(
                          y) == '*'
                     }
                     if n == 0 then ' ' else n.toString.head)
               .mkString)
         .toList
