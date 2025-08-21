class School {

  type DB = Map[Int, Seq[String]]
  var rooster: DB = Map(0 -> Seq("")).init

  def add(name: String, g: Int): Unit = {
    if (rooster.isEmpty || !rooster.keySet.contains(g)) rooster = rooster + (g -> Seq(name))
    else {
      rooster = rooster.updated(g, rooster.values.head ++ Seq(name))
    }
  }

  def db: DB = rooster

  def grade(g: Int): Seq[String] = {
    if (rooster.keySet.contains(g)) rooster.filter(x => x._1 == g).values.last
    else Seq()
  }

  def sorted: DB = {
    rooster.map(m => m._1 -> m._2.sorted).toSeq.sortBy(k => k._1).toMap
  }

}
