object Etl {

  def transform(m: Map[Int, Seq[String]]): Map[String, Int] = {
    m flatMap (t => t._2 map (s => (s.toLowerCase, t._1)))
  }
}

/*
  m flatMap (t => t._2 map (s => (s.toLowerCase, t._1)))

   for comprehension:

      {
        for (t <- m) yield {
          for (a <- t._2) yield {
            a.toLowerCase -> t._1
          }
        }
      }.flatten.toMap
    }
 */
