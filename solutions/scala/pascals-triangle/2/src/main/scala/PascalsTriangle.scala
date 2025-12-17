object PascalsTriangle:
  def rows(n: Int): List[List[Int]] =
    def nextRow(row: List[Int]): List[Int] =
      (0 +: row :+ 0).sliding(2).map(_.sum).toList

    Iterator.iterate(List(1))(nextRow).take(math.max(0, n)).toList
