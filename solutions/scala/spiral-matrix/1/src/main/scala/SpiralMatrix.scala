import scala.annotation.tailrec

object SpiralMatrix {

  private val ss: sides = sides(0, 1, 2, 3)

  def spiralMatrix(n: Int): List[List[Int]] = {
    val m = Array.ofDim[Int](n, n)

    run(1, 0, n - 1, 0, n - 1, n * n)

    @tailrec
    def run(value: Int, minCol: Int, maxCol: Int, minRow: Int, maxRow: Int, maxVal: Int): List[List[Int]] = {
      if (value > maxVal) Nil
      else {
        run(loop(maxRow, minCol, minRow + 1,
          loop(maxRow, maxCol, minCol,
            loop(minRow, maxCol, maxRow,
              loop(minRow, minCol, maxCol, value, ss.top, maxVal),
              ss.right, maxVal),
            ss.bottom, maxVal),
          ss.left, maxVal) + 1,
          minCol + 1, maxCol - 1, minRow + 1, maxRow - 1, maxVal)

      }
    }

    @tailrec
    def loop(row: Int, col: Int, end: Int, acc: Int, side: Int, max: Int): Int = side match {
      case ss.top =>
        m(row)(col) = acc
        if (col == end) acc
        else loop(row, col + 1, end, acc + 1, side, max)
      case ss.right =>
        m(row)(col) = acc
        if (row == end) acc
        else loop(row + 1, col, end, acc + 1, side, max)
      case ss.bottom =>
        m(row)(col) = acc
        if (col == end) acc
        else loop(row, col - 1, end, acc + 1, side, max)
      case ss.left =>
        m(row)(col) = acc
        if (row == end || acc == max) acc
        else loop(row - 1, col, end, acc + 1, side, max)
    }

    m.map(_.toList).toList
  }

  private case class sides(top: Int, right: Int, bottom: Int, left: Int)
}
