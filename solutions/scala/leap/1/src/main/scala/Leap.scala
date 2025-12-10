object Leap:
  def leapYear(year: Int): Boolean =
    if !(year % 4 == 0 && year % 100 != 0) && !(year % 100 == 0 && year % 400 == 0)
    then false
    else true
