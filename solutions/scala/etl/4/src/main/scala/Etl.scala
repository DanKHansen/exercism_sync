object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] = {

//flatMap version:

    m.map(x => x._2.flatMap(y => Map(y.toLowerCase -> x._1))).flatten.toMap

// for comprehension:
    //
    //    {
    //      for (t <- m) yield {
    //        for (a <- t._2) yield {
    //          a.toLowerCase -> t._1
    //        }
    //      }
    //    }.flatten.toMap
    //  }
  }
}