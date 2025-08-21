case class Matrix(s: String):
   val row: Array[Array[Int]] = s.split("\n").map(_.split("\\s").map(_.toInt))
   val column: Array[Array[Int]] = row.transpose