class School:
  private type DB = Map[Int, Seq[String]]

  private var gradeDB: DB = Map.empty

  def add(name: String, g: Int): Unit = gradeDB =
    gradeDB.updated(g, grade(g) :+ name)

  def db: DB = gradeDB

  def grade(g: Int): Seq[String] = gradeDB.getOrElse(g, Seq.empty)

  def sorted: DB =
    gradeDB.toSeq.sorted.toMap.map { case (k, v) => (k, v.sorted) }
