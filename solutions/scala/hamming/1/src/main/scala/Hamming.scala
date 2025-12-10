object Hamming:
  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] =

    (dnaStrandOne, dnaStrandTwo) match
      case (d1, d2) if d1 == d2                           => Some(0)
      case (d1, d2) if d1 != d2 && d1.length == d2.length =>
        Some(d1.zip(d2).count { case (a, b) => a != b })
      case _ => None
