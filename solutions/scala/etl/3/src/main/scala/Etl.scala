object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] = {
    {
      for (t <- m) yield {
        for (a <- t._2) yield {
          a.toLowerCase -> t._1
        }
      }
    }.flatten.toMap
  }
}