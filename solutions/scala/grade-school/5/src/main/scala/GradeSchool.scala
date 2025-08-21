class School {

  type DB = Map[Int, Seq[String]]
  var rooster: DB = Map.empty

  def add(name: String, g: Int): Unit = {
    if (rooster.isEmpty || grade(g).isEmpty)
      rooster = rooster + (g -> Seq(name))
    else
      rooster = rooster.updated(g, grade(g) ++ Seq(name))
  }

  def grade(g: Int): Seq[String] = rooster.getOrElse(g, Seq.empty)

  def db: DB = rooster

  def sorted: DB = rooster.map(m => m._1 -> m._2.sorted).toSeq.sortBy(m2 => m2._1).toMap

}
