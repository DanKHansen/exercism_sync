object ArmstrongNumbers:

  def isArmstrongNumber(n: Int): Boolean =
    val ds = n.toString.map(_.asDigit)
    ds.map(d => math.pow(d, ds.length).toInt).sum == n
