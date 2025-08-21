object HighScores {

  def personalTop(l: List[Int]): List[Int] = l.sortWith(_ > _).take(3)

  def report(l: List[Int]): String = {
    if (latest(l) == personalBest(l)) {
      s"Your latest score was ${latest(l)}. That's your personal best!"
    } else {
      s"Your latest score was ${latest(l)}. That's ${personalBest(l) - latest(l)} short of your personal best!"
    }
  }

  def latest(l: List[Int]): Int = l.last

  def personalBest(l: List[Int]): Int = l.max
}
