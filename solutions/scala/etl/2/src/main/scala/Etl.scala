object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] = {
    var nm: Map[String, Int] = Map()
    m.map(t => t._2.map(s => nm += (s.toLowerCase -> t._1)))
    nm
  }

}