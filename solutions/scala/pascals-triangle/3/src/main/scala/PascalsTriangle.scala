object PascalsTriangle:
  def rows(n: Int): List[List[Int]] =
    Iterator.iterate(List(1))(row => (0 +: row :+ 0).sliding(2).map(_.sum).toList)
      .take(math.max(0, n))
      .toList