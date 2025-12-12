case class Triangle(a: Double, b: Double, c: Double):
  private def isValid: Boolean = a + b >= c && b + c >= a && a + c >= b && a > 0 && b > 0 && c > 0
  def equilateral: Boolean = isValid && a == b && b == c
  def isosceles: Boolean = isValid && a == b || b == c || a == c
  def scalene: Boolean = isValid && !isosceles && !equilateral