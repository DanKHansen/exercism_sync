class School {

  type DB = Map[Int, Seq[String]]
  var rooster: DB = Map.empty

  def add(name: String, g: Int): Unit = {
    if (rooster.isEmpty || !rooster.keySet.contains(g)) rooster = rooster + (g -> Seq(name))
    else {
      rooster = rooster.updated(g, rooster.values.head ++ Seq(name))
    }
  }

  def db: DB = rooster

  def grade(g: Int): Seq[String] = {
   rooster.getOrElse(g,Seq.empty)
  }

  def sorted: DB = {
    rooster.map(m => m._1 -> m._2.sorted).toSeq.sortBy(m2 => m2._1).toMap
  }


}
