object HighScores {
  def latest(l: List[Int]): Int = l.last
  def personalBest(l: List[Int]): Int = l.max
  def personalTop(l: List[Int]): List[Int] = l.sortWith(_ > _).take(3)
  def report(l: List[Int]): String = {
    val x: (Int, Int, Int) = (latest(l), personalBest(l), personalBest(l) - latest(l))
    s"Your latest score was ${x._1}. That's ${if (x._3 == 0) ""
    else s"${x._3} short of "}your personal best!"}

}
