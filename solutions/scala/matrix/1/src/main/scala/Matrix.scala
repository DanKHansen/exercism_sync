case class Matrix(s: String):
   private val m = s.split("\n").map(_.split(" ").map(_.toInt).toVector).toVector
   def row(r: Int): Seq[Int] = m(r)
   def column(c: Int): Seq[Int] = m.map(_(c))