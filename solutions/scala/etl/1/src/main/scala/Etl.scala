object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] = {
    var newM: Map[String, Int] = Map()
    for (t <- m) yield {
      for (s <- t._2) yield {
        newM += s.toLowerCase -> t._1
      }
    }
    newM
  }
}