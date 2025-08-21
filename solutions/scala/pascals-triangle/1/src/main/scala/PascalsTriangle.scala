object PascalsTriangle {
  def rows(n: Int): List[List[Int]] = {
    (0 until n).map(r => (0 to r).map(c => pascal(c, r)).toList).toList
  }

  def pascal(c: Int, r: Int): Int = {
    if ((r == 0) || (r == c) || (c == 0)) 1 else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

}
