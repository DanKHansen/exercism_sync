object KillerSudokuHelper:
   def combinations(x: Int, y: Int, z: Seq[Int]): Seq[Seq[Int]] = Range(1,10).diff(z).combinations(y).filter(_.sum == x).toSeq