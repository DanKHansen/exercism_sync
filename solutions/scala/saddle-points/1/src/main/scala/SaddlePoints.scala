case class Matrix(l: Seq[Seq[Int]]):
   val saddlePoints: Set[(Int, Int)] =
      val cols = l.head.indices.map(j => l.map(_(j)))
      l.indices.flatMap(i => l(i).indices.filter(j => l(i)(j) == l(i).max && l(i)(j) == cols(j).min).map((i, _))).toSet
