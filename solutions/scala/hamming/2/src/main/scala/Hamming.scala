object Hamming {

  def distance(s1: String, s2: String): Option[Int] = {
    if (s1.length != s2.length) None
    else Some(s1.zip(s2).map(c => c._1 == c._2).count(_ == false))
  }

}