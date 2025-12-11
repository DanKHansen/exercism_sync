object ArmstrongNumbers:

  def isArmstrongNumber(n: Int): Boolean =
    n.toString.toList
      .map(c => math.pow(c.asDigit, n.toString.length).toInt)
      .sum
      == n
