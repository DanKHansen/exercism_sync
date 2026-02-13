object SpiralMatrix:
  def spiralMatrix(n: Int): List[List[Int]] =
    if n == 0 then Nil
    else
      (1 to n).toList :: spiralMatrix(n - 1).reverse.zipWithIndex.map { (row, i) =>
        row.reverse.map(_ + 2 * n - 1) ++ List(i + n + 1)
      }