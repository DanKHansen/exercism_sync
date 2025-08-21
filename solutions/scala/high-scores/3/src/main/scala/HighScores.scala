object HighScores {

  def personalTop(l: List[Int]): List[Int] = l.sortWith(_ > _).take(3)

  def report(l: List[Int]): String = s"Your latest score was ${latest(l)}. That's ${text(diff(l))}your personal best!"

  private def diff(l: List[Int]): Int = personalBest(l) - latest(l)

  def personalBest(l: List[Int]): Int = l.max

  def latest(l: List[Int]): Int = l.last

  private def text(i: Int): String = if (i == 0) "" else s"$i short of "
}
