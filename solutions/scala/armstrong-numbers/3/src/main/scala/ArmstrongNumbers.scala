object ArmstrongNumbers:

  def isArmstrongNumber(n: Int): Boolean =
    val s = n.toString
    s.map(c => math.pow(c.asDigit, s.length).toInt).sum == n
