object Etl:
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] =
    scoreMap.flatMap { case (i, cs) => cs.map(_.toLowerCase -> i) }
