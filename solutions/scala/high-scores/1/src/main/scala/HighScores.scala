object HighScores {

  def personalTop(l: List[Int]): List[Int] = l.sorted.reverse.take(3)

  def report(l: List[Int]): String = {
    val p: (Int, Int) = (latest(l), personalBest(l))
    if (p._1 == p._2) {
      "Your latest score was " + p._1 + ". That's your personal best!"
    } else {
      "Your latest score was " + p._1 + ". That's " + {
        p._2 - p._1
      } + " short of your personal best!"
    }
  }

  def latest(l: List[Int]): Int = l.reverse.head

  def personalBest(l: List[Int]): Int = l.max
}
